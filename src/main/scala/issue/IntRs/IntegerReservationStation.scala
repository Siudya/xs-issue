package issue.IntRs
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import common.{MicroOp, Redirect, XSParam}
import exu.ExuType
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import issue._

class IntegerReservationStation(implicit p: Parameters) extends LazyModule with XSParam{

  private val bankNum = 4
  private val issueNum = 7
  private val wakeupNum = 1
  val enqNode = new RsDispatchNode(Seq(DispatchParam("Integer", bankNum)))
  val issueNode = new RsIssueNode(Seq.fill(issueNum)(None))
  val wakeupNode = new RsWakeupNode(Seq.fill(wakeupNum)(None))
  private val rsParam = RsParam(48, bankNum)
  lazy val module = new IntegerReservationStationImpl(this, rsParam)
}
class IntegerReservationStationImpl(outer:IntegerReservationStation, param:RsParam) extends LazyModuleImp(outer) with XSParam {
  require(param.bankNum == 4)
  require(param.entriesNum % param.bankNum == 0)
  private val enq = outer.enqNode.in
  private val issue = outer.issueNode.out
  private val wakeup = outer.wakeupNode.in
  issue.foreach(elm => require(ExuType.intType.contains(elm._2.exuType)))
  private val jmpIssue = issue.filter(_._2.exuType == ExuType.jmp)
  private val aluIssue = issue.filter(_._2.exuType == ExuType.alu)
  private val mulIssue = issue.filter(_._2.exuType == ExuType.mul)
  private val divIssue = issue.filter(_._2.exuType == ExuType.div)
  private val issuePortList = List(jmpIssue, aluIssue, mulIssue, divIssue)
  require(jmpIssue.nonEmpty && jmpIssue.length <= param.bankNum && (param.bankNum % jmpIssue.length) == 0)
  require(aluIssue.nonEmpty && aluIssue.length <= param.bankNum && (param.bankNum % aluIssue.length) == 0)
  require(mulIssue.nonEmpty && mulIssue.length <= param.bankNum && (param.bankNum % mulIssue.length) == 0)
  require(divIssue.nonEmpty && divIssue.length <= param.bankNum && (param.bankNum % divIssue.length) == 0)
  private val issueTypeNum = ExuType.intType.length
  private val issueNumForEachFu = issuePortList.map(_.length)
  private val internalWakeupNum = issue.count(elm => elm._2.latency != Int.MaxValue)
  private val entriesNumPerBank = param.entriesNum / param.bankNum

  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val specWakeup = Output(Vec(internalWakeupNum, Valid(new WakeUpInfo)))
  })

  private val internalWakeup = Wire(Vec(internalWakeupNum, Valid(new WakeUpInfo)))
  io.specWakeup := internalWakeup
  private var internalWakeupPtr = 0

  private val wakeupSignals = VecInit(wakeup.map(_._1))
  private val rsBankSeq = Seq.tabulate(param.bankNum)( _ => {
    val mod = Module(new IntegerReservationStationBank(entriesNumPerBank, issueTypeNum, internalWakeupNum + wakeup.length, loadUnitNum))
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
    if(ExuType.maybeBlockType.contains(elm.head._2.exuType)){
      for((sink, source) <- mod.io.tokenRelease.get.zip(elm)) {
        sink := source._1.release
      }
    }
    if(elm.head._2.latency != Int.MaxValue){
      val wkq = Seq.fill(snIssueNum)(Module(new WakeupQueue(elm.head._2.latency)))
      for(((q, sink), source) <- wkq.zip(internalWakeup.slice(internalWakeupPtr, internalWakeupPtr + snIssueNum)).zip(mod.io.issueInfo)){
        q.io.redirect := io.redirect
        q.io.in.valid := source.valid & source.bits.info.rfWen
        q.io.in.bits.lpv := source.bits.info.lpv
        q.io.in.bits.robPtr := source.bits.info.robPtr
        q.io.in.bits.pdest := source.bits.info.pdest
        sink := q.io.out
      }
      internalWakeupPtr = internalWakeupPtr + snIssueNum
    }
    mod
  })

  allocateNetwork.io.enqFromDispatch.zip(enq.head._1).foreach({case(sink, source) =>
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

  private val issRegs = Seq.tabulate(issueTypeNum)(idx =>
    RegInit(VecInit(Seq.tabulate(issueNumForEachFu(idx))(_ => 0.U.asTypeOf(Valid(new MicroOp)))))
  )

  for((((iss, issR), sn), fuIdx) <- issuePortList.zip(issRegs).zip(selectNetworkSeq).zipWithIndex){
    for(((iss_elm, issR_elm), portIdx) <- iss.zip(issR).zipWithIndex){
      val issueBundle = Wire(Valid(new MicroOp))
      val rbIssAddrPorts = rsBankSeq.map(_.io.issueAddr(fuIdx))
      val rbIssDataPorts = rsBankSeq.map(_.io.issueData(fuIdx))
      val bn = param.bankNum / iss.length
      val rbAddrPortsForThisPort = rbIssAddrPorts.slice(portIdx * bn, portIdx * bn + bn)
      val rbDataPortsForThisPort = rbIssDataPorts.slice(portIdx * bn, portIdx * bn + bn)
      val selectResps = sn.io.issueInfo(portIdx)

      issueBundle.valid := selectResps.valid
      issueBundle.bits := Mux1H(rbDataPortsForThisPort.map(elm => (elm.valid, elm.bits)))
      issueBundle.bits.robIdx := selectResps.bits.info.robPtr
      issueBundle.bits.ctrl.rfWen := selectResps.bits.info.rfWen
      issueBundle.bits.ctrl.rfWen := selectResps.bits.info.fpWen
      issueBundle.bits.pdest := selectResps.bits.info.pdest
      issueBundle.bits.ctrl.fuType := selectResps.bits.info.fuType
      issueBundle.bits.lpv := selectResps.bits.info.lpv

      issR_elm.valid := issueBundle.valid
      when(issueBundle.valid) {
        issR_elm.bits := issueBundle.bits
      }
      iss_elm._1.uop.valid := issR_elm.valid
      iss_elm._1.uop.bits := issR_elm.bits

      rbAddrPortsForThisPort.zipWithIndex.foreach({case(rb,bidx) =>
        rb.valid := sn.io.issueInfo.head.valid & sn.io.issueInfo.head.bits.bankIdxOH(bidx + portIdx * bn)
        rb.bits := sn.io.issueInfo.head.bits.entryIdxOH
      })
    }
  }
}
