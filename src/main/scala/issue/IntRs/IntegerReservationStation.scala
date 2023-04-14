package issue.IntRs
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import common.{MicroOp, Redirect, XSParam}
import exu.ExuType
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import issue._
import writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}

class IntegerReservationStation(bankNum:Int)(implicit p: Parameters) extends LazyModule with XSParam{
  private val issueNum = 8
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
        sink := source._1.feedback.release
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

  private val issRegs = issuePortList.map(p =>
    Reg(MixedVec(p.map(n => new IssueBundle(n._2.releaseWidth))))
  )

  for((((iss, issR), sn), fuIdx) <- issuePortList.zip(issRegs).zip(selectNetworkSeq).zipWithIndex){
    for(((iss_elm, issR_elm), portIdx) <- iss.zip(issR).zipWithIndex){
      val issueBundle = Wire(new IssueBundle(iss_elm._2.releaseWidth))
      issueBundle := DontCare
      val rbIssAddrPorts = rsBankSeq.map(_.io.issueAddr(fuIdx))
      val rbIssDataPorts = rsBankSeq.map(_.io.issueData(fuIdx))
      val bn = param.bankNum / iss.length
      val rbAddrPortsForThisPort = rbIssAddrPorts.slice(portIdx * bn, portIdx * bn + bn)
      val rbDataPortsForThisPort = rbIssDataPorts.slice(portIdx * bn, portIdx * bn + bn)
      val selectResps = sn.io.issueInfo(portIdx)

      issueBundle.issue.valid := selectResps.valid
      issueBundle.issue.uop := Mux1H(rbDataPortsForThisPort.map(elm => (elm.valid, elm.bits)))
      issueBundle.issue.uop.robIdx := selectResps.bits.info.robPtr
      issueBundle.issue.uop.ctrl.rfWen := selectResps.bits.info.rfWen
      issueBundle.issue.uop.ctrl.fpWen := selectResps.bits.info.fpWen
      issueBundle.issue.uop.pdest := selectResps.bits.info.pdest
      issueBundle.issue.uop.ctrl.fuType := selectResps.bits.info.fuType
      issueBundle.issue.uop.lpv := selectResps.bits.info.lpv
      issueBundle.issue.fuSel := selectResps.bits.fuSel

      val issValidDriverRegs = RegInit(false.B)
      val issuePermitted = (!issValidDriverRegs) || iss_elm._1.fire
      when(issuePermitted) {
        issValidDriverRegs := issueBundle.issue.valid
      }
      when(issuePermitted && issueBundle.issue.valid) {
        issR_elm := issueBundle
      }
      iss_elm._1.issue := issR_elm
      iss_elm._1.issue.valid := issValidDriverRegs

      rbAddrPortsForThisPort.zipWithIndex.foreach({case(rb,bidx) =>
        rb.valid := sn.io.issueInfo.head.valid & sn.io.issueInfo.head.bits.bankIdxOH(bidx + portIdx * bn)
        rb.bits := sn.io.issueInfo.head.bits.entryIdxOH
      })
    }
  }
}
