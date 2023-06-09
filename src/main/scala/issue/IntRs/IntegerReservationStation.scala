package issue.IntRs
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import common.{FuType, MicroOp, Redirect, SrcType, XSParam}
import execute.exu.ExuType
import execute.exu.ExuType.intTypes
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, ValName}
import issue._
import writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}
import xs.utils.Assertion.xs_assert


class IntegerReservationStation(bankNum:Int, entryNum:Int)(implicit p: Parameters) extends LazyModule with XSParam{
  require(entryNum % bankNum == 0)
  private val wbNodeParam = WriteBackSinkParam(name = "Integer RS", sinkType = WriteBackSinkType.intRs)
  private val rsParam = RsParam(name = "Integer RS", RsType.int, entryNum, bankNum)
  val issueNode = new RsIssueNode(rsParam)
  val wakeupNode = new WriteBackSinkNode(wbNodeParam)

  lazy val module = new IntegerReservationStationImpl(this, rsParam)
}

class IntegerReservationStationImpl(outer:IntegerReservationStation, param:RsParam) extends LazyModuleImp(outer) with XSParam {
  require(param.bankNum == 4)
  require(param.entriesNum % param.bankNum == 0)
  private val issue = outer.issueNode.out.head._1 zip outer.issueNode.out.head._2._2
  private val wbIn = outer.wakeupNode.in.head
  private val wakeup = wbIn._1.zip(wbIn._2)
  issue.foreach(elm => elm._2.exuConfigs.foreach(elm0 => require(ExuType.intTypes.contains(elm0.exuType))))
  private val aluIssue = issue.filter(_._2.hasAlu)
  private val mulIssue = issue.filter(_._2.hasMul)
  private val divIssue = issue.filter(_._2.hasDiv)
  private val jmpIssue = issue.filter(_._2.hasJmp)

  private val aluIssuePortNum = issue.count(_._2.hasAlu)
  private val mulIssuePortNum = issue.count(_._2.hasMul)
  private val divIssuePortNum = issue.count(_._2.hasDiv)
  private val jmpIssuePortNum = issue.count(_._2.hasJmp)

  require(aluIssue.nonEmpty && aluIssue.length <= param.bankNum && (param.bankNum % aluIssue.length) == 0)
  require(mulIssue.nonEmpty && mulIssue.length <= param.bankNum && (param.bankNum % mulIssue.length) == 0)
  require(divIssue.nonEmpty && divIssue.length <= param.bankNum && (param.bankNum % divIssue.length) == 0)
  require(jmpIssue.nonEmpty && jmpIssue.length <= param.bankNum && (param.bankNum % jmpIssue.length) == 0)

  private val issueWidth = issue.length
  private val entriesNumPerBank = param.entriesNum / param.bankNum

  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val enq = Vec(param.bankNum, Flipped(DecoupledIO(new MicroOp)))
    val specWakeup = Output(Vec(aluIssuePortNum + mulIssuePortNum, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
  })

  private val internalWakeupSignals = Wire(Vec(aluIssuePortNum + mulIssuePortNum, Valid(new WakeUpInfo)))
  io.specWakeup := internalWakeupSignals

  private val wakeupSignals = VecInit(wakeup.map(_._1).map(elm =>{
    val wkp = Wire(Valid(new WakeUpInfo))
    wkp.valid := elm.valid
    wkp.bits.pdest := elm.bits.uop.pdest
    wkp.bits.robPtr := elm.bits.uop.robIdx
    wkp.bits.lpv := 0.U.asTypeOf(wkp.bits.lpv)
    wkp.bits.destType := Mux(elm.bits.uop.ctrl.rfWen, SrcType.reg, SrcType.default)
    wkp
  }))
  private val rsBankSeq = Seq.tabulate(param.bankNum)( _ => {
    val mod = Module(new IntegerReservationBank(entriesNumPerBank, issueWidth, (wakeupSignals ++ internalWakeupSignals).length, loadUnitNum))
    mod.io.redirect := io.redirect
    mod.io.wakeup := wakeupSignals ++ internalWakeupSignals
    mod.io.loadEarlyWakeup := io.loadEarlyWakeup
    mod.io.earlyWakeUpCancel := io.earlyWakeUpCancel
    mod
  })
  private val allocateNetwork = Module(new AllocateNetwork(param.bankNum, entriesNumPerBank, Some("IntegerAllocateNetwork")))

  private val aluExuCfg = aluIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.alu).head
  private val mulExuCfg = mulIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.mul).head
  private val divExuCfg = divIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.div).head
  private val jmpExuCfg = jmpIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.jmp).head

  private val aluSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, aluIssuePortNum, aluExuCfg, Some(s"IntegerAluSelectNetwork")))
  private val mulSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, mulIssuePortNum, mulExuCfg, Some(s"IntegerMulSelectNetwork")))
  private val divSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, divIssuePortNum, divExuCfg, Some(s"IntegerDivSelectNetwork")))
  private val jmpSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, jmpIssuePortNum, jmpExuCfg, Some(s"IntegerJmpSelectNetwork")))
  divSelectNetwork.io.tokenRelease.get.zip(wakeup.filter(_._2.exuType == ExuType.div).map(_._1)).foreach({
    case(sink, source) =>
      sink.valid := source.valid && source.bits.uop.ctrl.rfWen
      sink.bits := source.bits.uop.pdest
  })
  private val selectNetworkSeq = Seq(aluSelectNetwork, mulSelectNetwork, divSelectNetwork, jmpSelectNetwork)
  selectNetworkSeq.foreach(sn => {
    sn.io.selectInfo.zip(rsBankSeq).foreach({ case (sink, source) =>
      sink := source.io.selectInfo
    })
    sn.io.redirect := io.redirect
  })

  allocateNetwork.io.enqFromDispatch.zip(io.enq).foreach({case(sink, source) =>
    sink.valid := source.valid
    sink.bits := source.bits
    source.ready := sink.ready
    xs_assert(Mux(source.valid, FuType.integerTypes.map(_ === source.bits.ctrl.fuType).reduce(_||_), true.B))
  })

  for(((fromAllocate, toAllocate), rsBank) <- allocateNetwork.io.enqToRs
    .zip(allocateNetwork.io.entriesValidBitVecList)
    .zip(rsBankSeq)){
    toAllocate := rsBank.io.allocateInfo
    rsBank.io.enq.valid := fromAllocate.valid
    rsBank.io.enq.bits.data := fromAllocate.bits.uop
    rsBank.io.enq.bits.addrOH := fromAllocate.bits.addrOH
  }

  private var internalWkpPortIdx = 0
  private var aluPortIdx = 0
  private var mulPortIdx = 0
  private var divPortIdx = 0
  private var jmpPortIdx = 0
  println("\nInteger Reservation Issue Ports Config:")
  for((iss, issuePortIdx) <- issue.zipWithIndex) {
    println(s"Issue Port $issuePortIdx ${iss._2}")
    prefix(iss._2.name + "_" + iss._2.id) {
      val issueDriver = Module(new DecoupledPipeline(false, param.bankNum, entriesNumPerBank))
      issueDriver.io.redirect := io.redirect
      issueDriver.io.earlyWakeUpCancel := io.earlyWakeUpCancel
      val selectRespArbiter = Module(new SelectRespArbiter(param.bankNum, entriesNumPerBank, 2))
      selectRespArbiter.io.in(0) <> aluSelectNetwork.io.issueInfo(aluPortIdx)
      internalWakeupSignals(internalWkpPortIdx) := WakeupQueue(aluSelectNetwork.io.issueInfo(aluPortIdx), aluSelectNetwork.cfg.latency, io.redirect, io.earlyWakeUpCancel)
      aluPortIdx = aluPortIdx + 1
      internalWkpPortIdx = internalWkpPortIdx + 1
      if (iss._2.isAluMul) {
        selectRespArbiter.io.in(1) <> mulSelectNetwork.io.issueInfo(mulPortIdx)
        internalWakeupSignals(internalWkpPortIdx) := WakeupQueue(mulSelectNetwork.io.issueInfo(mulPortIdx), mulSelectNetwork.cfg.latency, io.redirect, io.earlyWakeUpCancel)
        mulPortIdx = mulPortIdx + 1
        internalWkpPortIdx = internalWkpPortIdx + 1
      } else if (iss._2.isAluDiv) {
        selectRespArbiter.io.in(1) <> divSelectNetwork.io.issueInfo(divPortIdx)
        divPortIdx = divPortIdx + 1
      } else if(iss._2.isAluJmp){
        selectRespArbiter.io.in(1) <> jmpSelectNetwork.io.issueInfo(jmpPortIdx)
        jmpPortIdx = jmpPortIdx + 1
      } else {
        require(false, "Unknown Exu complex!")
      }
      val finalSelectInfo = selectRespArbiter.io.out
      val rsBankRen = Mux(issueDriver.io.enq.fire, finalSelectInfo.bits.bankIdxOH, 0.U)
      rsBankSeq.zip(rsBankRen.asBools).foreach({ case (rb, ren) =>
        rb.io.issueAddr(issuePortIdx).valid := ren
        rb.io.issueAddr(issuePortIdx).bits := finalSelectInfo.bits.entryIdxOH
      })

      val issueBundle = Wire(Valid(new MicroOp))
      issueBundle.valid := finalSelectInfo.valid
      issueBundle.bits := Mux1H(rsBankRen, rsBankSeq.map(_.io.issueUop(issuePortIdx).bits))
      issueBundle.bits.robIdx := finalSelectInfo.bits.info.robPtr
      issueBundle.bits.ctrl.rfWen := finalSelectInfo.bits.info.rfWen
      issueBundle.bits.ctrl.fpWen := finalSelectInfo.bits.info.fpWen
      issueBundle.bits.pdest := finalSelectInfo.bits.info.pdest
      issueBundle.bits.ctrl.fuType := finalSelectInfo.bits.info.fuType
      issueBundle.bits.lpv := finalSelectInfo.bits.info.lpv

      finalSelectInfo.ready := issueDriver.io.enq.ready
      issueDriver.io.enq.valid := issueBundle.valid
      issueDriver.io.enq.bits.uop := issueBundle.bits
      issueDriver.io.enq.bits.fmaMidStateIssue.valid := false.B
      issueDriver.io.enq.bits.fmaMidStateIssue.bits := DontCare
      issueDriver.io.enq.bits.fmaWaitForAdd := false.B
      issueDriver.io.enq.bits.bankIdxOH := finalSelectInfo.bits.bankIdxOH
      issueDriver.io.enq.bits.entryIdxOH := finalSelectInfo.bits.entryIdxOH

      iss._1.issue.valid := issueDriver.io.deq.valid
      iss._1.issue.bits.uop := issueDriver.io.deq.bits.uop
      iss._1.issue.bits.src := DontCare
      iss._1.fmaMidState.in.valid := false.B
      iss._1.fmaMidState.in.bits := DontCare
      iss._1.fmaMidState.waitForAdd := false.B
      iss._1.rsIdx.bankIdxOH := issueDriver.io.deq.bits.bankIdxOH
      iss._1.rsIdx.entryIdxOH := issueDriver.io.deq.bits.entryIdxOH
      issueDriver.io.deq.ready := iss._1.issue.ready
    }
  }
  println("\nInteger Reservation Wake Up Ports Config:")
  wakeup.zipWithIndex.foreach({case((_, cfg), idx) =>
    println(s"Wake Port $idx ${cfg.name} of ${cfg.complexName} #${cfg.id}")
  })
}
