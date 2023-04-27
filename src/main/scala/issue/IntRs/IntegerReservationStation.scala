package issue.IntRs
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import common.{MicroOp, Redirect, XSParam}
import execute.exu.ExuType
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, ValName}
import execute.fu.fpu.FMAMidResult
import issue.FpRs.{DecoupledPipeline, FloatingReservationBank, MidStateWaitQueue}
import issue._
import writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}


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
  println("Floating Reservation Issue Ports Config:")
  outer.issueNode.out.head._2._2.foreach(cfg => println(cfg))
  private val wbIn = outer.wakeupNode.in.head
  private val wakeup = wbIn._1.zip(wbIn._2)
  issue.foreach(elm => elm._2.exuConfigs.foreach(elm0 => require(ExuType.intTypes.contains(elm0.exuType))))

  private val aluIssue = issue.filter(_._2.hasAlu)
  private val mulIssue = issue.filter(_._2.hasMul)
  private val i2fIssue = issue.filter(_._2.hasI2f)
  private val divIssue = issue.filter(_._2.hasDiv)
  private val jmpIssue = issue.filter(_._2.hasJmp)

  private val aluIssuePortNum = issue.count(_._2.hasAlu)
  private val mulIssuePortNum = issue.count(_._2.hasMul)
  private val i2fIssuePortNum = issue.count(_._2.hasI2f)
  private val divIssuePortNum = issue.count(_._2.hasDiv)
  private val jmpIssuePortNum = issue.count(_._2.hasJmp)

  require(aluIssue.nonEmpty && aluIssue.length <= param.bankNum && (param.bankNum % aluIssue.length) == 0)
  require(mulIssue.nonEmpty && mulIssue.length <= param.bankNum && (param.bankNum % mulIssue.length) == 0)
  require(i2fIssue.nonEmpty && i2fIssue.length <= param.bankNum && (param.bankNum % i2fIssue.length) == 0)
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
  io.enq.suggestName("new_enq")


  private val internalWakeupSignals = Wire(Vec(aluIssuePortNum + mulIssuePortNum, Valid(new WakeUpInfo)))
  io.specWakeup := internalWakeupSignals

  private val wakeupSignals = VecInit(wakeup.map(_._1).map(elm =>{
    val wkp = Wire(Valid(new WakeUpInfo))
    wkp.valid := elm.valid
    wkp.bits.pdest := elm.bits.uop.pdest
    wkp.bits.robPtr := elm.bits.uop.robIdx
    wkp.bits.lpv := 0.U.asTypeOf(wkp.bits.lpv)
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
  private val i2fExuCfg = i2fIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.i2f).head
  private val divExuCfg = divIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.div).head
  private val jmpExuCfg = jmpIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.jmp).head

  private val aluSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, aluIssuePortNum, aluExuCfg, Some(s"IntegerAluSelectNetwork")))
  private val mulSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, mulIssuePortNum, mulExuCfg, Some(s"IntegerMulSelectNetwork")))
  private val i2fSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, i2fIssuePortNum, i2fExuCfg, Some(s"IntegerI2fSelectNetwork")))
  private val divSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, divIssuePortNum, divExuCfg, Some(s"IntegerDivSelectNetwork")))
  private val jmpSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, jmpIssuePortNum, jmpExuCfg, Some(s"IntegerJmpSelectNetwork")))
  divSelectNetwork.io.tokenRelease.get.zip(wakeup.filter(_._2.exuType == ExuType.div).map(_._1)).foreach({
    case(sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits.uop.pdest
  })
  private val selectNetworkSeq = Seq(aluSelectNetwork, mulSelectNetwork, i2fSelectNetwork, divSelectNetwork, jmpSelectNetwork)
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
  private var i2fPortIdx = 0
  private var divPortIdx = 0
  private var jmpPortIdx = 0
  for((iss, issuePortIdx) <- issue.zipWithIndex) {
    prefix(iss._2.name + "_" + iss._2.id) {
      val issueDriver = Module(new DecoupledPipeline(iss._2.isJmpCsr, param.bankNum, entriesNumPerBank))
      issueDriver.io.redirect := io.redirect

      val finalSelectInfo = if (iss._2.isJmpCsr) {
        jmpPortIdx = jmpPortIdx + 1
        jmpSelectNetwork.io.issueInfo(jmpPortIdx - 1)
      } else if (iss._2.isAluMul) {
        aluPortIdx = aluPortIdx + 1
        mulPortIdx = mulPortIdx + 1
        internalWkpPortIdx = internalWkpPortIdx + 2
        val selectRespArbiter = Module(new Arbiter(new SelectResp(param.bankNum, entriesNumPerBank), 2))
        selectRespArbiter.io.in(0) <> aluSelectNetwork.io.issueInfo(aluPortIdx - 1)
        selectRespArbiter.io.in(1) <> mulSelectNetwork.io.issueInfo(mulPortIdx - 1)
        internalWakeupSignals(internalWkpPortIdx - 2) := WakeupQueue(aluSelectNetwork.io.issueInfo(aluPortIdx - 1), aluSelectNetwork.cfg.latency, io.redirect)
        internalWakeupSignals(internalWkpPortIdx - 1) := WakeupQueue(mulSelectNetwork.io.issueInfo(mulPortIdx - 1), mulSelectNetwork.cfg.latency, io.redirect)
        selectRespArbiter.io.out
      } else if (iss._2.isAluDiv) {
        aluPortIdx = aluPortIdx + 1
        divPortIdx = divPortIdx + 1
        internalWkpPortIdx = internalWkpPortIdx + 1
        val selectRespArbiter = Module(new Arbiter(new SelectResp(param.bankNum, entriesNumPerBank), 2))
        selectRespArbiter.io.in(0) <> aluSelectNetwork.io.issueInfo(aluPortIdx - 1)
        selectRespArbiter.io.in(1) <> divSelectNetwork.io.issueInfo(divPortIdx - 1)
        internalWakeupSignals(internalWkpPortIdx - 1) := WakeupQueue(aluSelectNetwork.io.issueInfo(aluPortIdx - 1), aluSelectNetwork.cfg.latency, io.redirect)
        selectRespArbiter.io.out
      } else {
        aluPortIdx = aluPortIdx + 1
        i2fPortIdx = i2fPortIdx + 1
        internalWkpPortIdx = internalWkpPortIdx + 1
        val selectRespArbiter = Module(new Arbiter(new SelectResp(param.bankNum, entriesNumPerBank), 2))
        selectRespArbiter.io.in(0) <> aluSelectNetwork.io.issueInfo(aluPortIdx - 1)
        selectRespArbiter.io.in(1) <> i2fSelectNetwork.io.issueInfo(i2fPortIdx - 1)
        internalWakeupSignals(internalWkpPortIdx - 1) := WakeupQueue(aluSelectNetwork.io.issueInfo(aluPortIdx - 1), aluSelectNetwork.cfg.latency, io.redirect)
        selectRespArbiter.io.out
      }

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
}
