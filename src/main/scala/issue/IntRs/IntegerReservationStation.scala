package issue.IntRs
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import common.{MicroOp, Redirect, XSParam}
import exu.ExuType
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, ValName}
import freechips.rocketchip.macros.ValNameImpl
import issue._
import writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}

class IntegerReservationStation(bankNum:Int)(implicit p: Parameters) extends LazyModule with XSParam{
  private val wbNodeParam = WriteBackSinkParam(name = "Integer RS", sinkType = WriteBackSinkType.intRs)
  private val rsParam = RsParam(name = "Integer RS", RsType.int, 48, bankNum)
  val issueNode = new RsIssueNode(rsParam)
  val wakeupNode = new WriteBackSinkNode(wbNodeParam)

  lazy val module = new IntegerReservationStationImpl(this, rsParam)
}

class IntegerReservationStationImpl(outer:IntegerReservationStation, param:RsParam) extends LazyModuleImp(outer) with XSParam {
  require(param.bankNum == 4)
  require(param.entriesNum % param.bankNum == 0)
  private val issue = outer.issueNode.out.head._1 zip outer.issueNode.out.head._2
  println("Integer Reservation Issue Ports Config:")
  outer.issueNode.out.head._2.foreach(cfg => println(cfg))
  private val wbIn = outer.wakeupNode.in.head
  private val wakeup = wbIn._1.zip(wbIn._2)
  issue.foreach(elm => require(ExuType.intTypes.contains(elm._2.exuType)))
  private val jmpIssue = issue.filter(_._2.exuType == ExuType.jmp)
  private val aluIssue = issue.filter(_._2.exuType == ExuType.alu)
  private val mulIssue = issue.filter(_._2.exuType == ExuType.mul)
  private val divIssue = issue.filter(_._2.exuType == ExuType.div)
  private val issuePortList = List(jmpIssue, aluIssue, mulIssue, divIssue)
  require(jmpIssue.nonEmpty && jmpIssue.length <= param.bankNum && (param.bankNum % jmpIssue.length) == 0)
  require(aluIssue.nonEmpty && aluIssue.length <= param.bankNum && (param.bankNum % aluIssue.length) == 0)
  require(mulIssue.nonEmpty && mulIssue.length <= param.bankNum && (param.bankNum % mulIssue.length) == 0)
  require(divIssue.nonEmpty && divIssue.length <= param.bankNum && (param.bankNum % divIssue.length) == 0)
  private val issueTypeNum = ExuType.intTypes.length
  private val issueNumForEachFu = issuePortList.map(_.length)
  private val internalWakeupNum = issue.count(elm => elm._2.latency != Int.MaxValue)
  private val entriesNumPerBank = param.entriesNum / param.bankNum

  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val enq = Vec(param.bankNum, Flipped(DecoupledIO(new MicroOp)))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val specWakeup = Output(Vec(internalWakeupNum, Valid(new WakeUpInfo)))
  })
  io.enq.suggestName("new_enq")

  private val internalWakeup = Wire(Vec(internalWakeupNum, Valid(new WakeUpInfo)))
  io.specWakeup := internalWakeup
  private var internalWakeupPtr = 0

  private val wakeupSignals = VecInit(wakeup.map(_._1).map(elm =>{
    val wkp = Wire(Valid(new WakeUpInfo))
    wkp.valid := elm.valid
    wkp.bits.pdest := elm.bits.uop.pdest
    wkp.bits.robPtr := elm.bits.uop.robIdx
    wkp.bits.lpv := 0.U.asTypeOf(wkp.bits.lpv)
    wkp
  }))
  private val rsBankSeq = Seq.tabulate(param.bankNum)( _ => {
    val mod = Module(new IntegerReservationBank(entriesNumPerBank, issueTypeNum, internalWakeupNum + wakeup.length, loadUnitNum))
    mod.io.redirect := io.redirect
    mod.io.wakeup := wakeupSignals ++ internalWakeup
    mod.io.loadEarlyWakeup := io.loadEarlyWakeup
    mod.io.earlyWakeUpCancel := io.earlyWakeUpCancel
    mod
  })
  private val allocateNetwork = Module(new AllocateNetwork(param.bankNum, entriesNumPerBank, Some("IntegerAllocateNetwork")))
  private val selectNetworkSeq = issuePortList.map(elm => {
    val snIssueNum = elm.length
    val snName = elm.head._2.name
    val snCfg = elm.head._2
    val mod = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, snIssueNum, snCfg, Some(s"Integer${snName}SelectNetwork")))
    mod.io.redirect := io.redirect
    if(elm.head._2.latency != Int.MaxValue){
      val wkq = Seq.fill(snIssueNum)(Module(new WakeupQueue(elm.head._2.latency)))
      for(((q, sink), source) <- wkq.zip(internalWakeup.slice(internalWakeupPtr, internalWakeupPtr + snIssueNum)).zip(mod.io.issueInfo)){
        q.io.redirect := io.redirect
        q.io.in.valid := source.valid & source.bits.info.rfWen
        q.io.in.bits.lpv := source.bits.info.lpv
        q.io.in.bits.robPtr := source.bits.info.robPtr
        q.io.in.bits.pdest := source.bits.info.pdest
        sink := q.io.out
        if(snCfg.exuType == ExuType.mul){
          //Latency of Mul is reduce by bypass, but no bypass to other block. Add an reg here.
          sink := Pipe(q.io.out, 1)
        }
      }
      internalWakeupPtr = internalWakeupPtr + snIssueNum
    }
    mod
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

  selectNetworkSeq.foreach(sn => {
    sn.io.selectInfo.zip(rsBankSeq).foreach({case(sink, source) =>
      sink := source.io.selectInfo
    })
  })

  for(((iss, sn), fuIdx) <- issuePortList.zip(selectNetworkSeq).zipWithIndex){
    val rbIssAddrPorts = rsBankSeq.map(_.io.issueAddr(fuIdx))
    val rbIssDataPorts = rsBankSeq.map(_.io.issueData(fuIdx))
    for((iss_elm, portIdx) <- iss.zipWithIndex) {
      prefix(s"${iss_elm._2.name}_${iss_elm._2.id}") {
        val issueBundle = Wire(Valid(new MicroOp))
        val bn = param.bankNum / iss.length
        val rbAddrPortsForThisPort = rbIssAddrPorts.slice(portIdx * bn, portIdx * bn + bn)
        val rbDataPortsForThisPort = rbIssDataPorts.slice(portIdx * bn, portIdx * bn + bn)
        val selectResps = sn.io.issueInfo(portIdx)

        issueBundle := DontCare
        issueBundle.valid := selectResps.valid
        issueBundle.bits := Mux1H(rbDataPortsForThisPort.map(elm => (elm.valid, elm.bits)))
        issueBundle.bits.robIdx := selectResps.bits.info.robPtr
        issueBundle.bits.ctrl.rfWen := selectResps.bits.info.rfWen
        issueBundle.bits.ctrl.fpWen := selectResps.bits.info.fpWen
        issueBundle.bits.pdest := selectResps.bits.info.pdest
        issueBundle.bits.ctrl.fuType := selectResps.bits.info.fuType
        issueBundle.bits.lpv := selectResps.bits.info.lpv

        val issueValidDriverReg = RegInit(false.B)
        val issueBitsDriverReg = Reg(new MicroOp)
        val issueAllow = !issueValidDriverReg || iss_elm._1.issue.fire
        when(issueAllow){
          issueValidDriverReg := issueBundle.valid
        }
        when(issueAllow && issueBundle.valid){
          issueBitsDriverReg := issueBundle.bits
        }

        iss_elm._1.fmaMidState.in.valid := false.B
        iss_elm._1.fmaMidState.in.bits := DontCare
        iss_elm._1.fmaMidState.waitForAdd := false.B
        iss_elm._1.issue.valid := issueValidDriverReg
        iss_elm._1.issue.bits.uop := issueBitsDriverReg

        rbAddrPortsForThisPort.zipWithIndex.foreach({ case (rb, bidx) =>
          rb.valid := selectResps.bits.bankIdxOH(bidx + portIdx * bn) & issueAllow
          rb.bits := selectResps.bits.entryIdxOH
        })
      }
    }
  }
}
