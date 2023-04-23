package issue.FpRs
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import common.{MicroOp, Redirect, XSParam}
import exu.ExuType
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, ValName}
import freechips.rocketchip.macros.ValNameImpl
import fu.fpu.FMAMidResult
import issue._
import regfile.DecoupledPipeline
import writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}
import xs.utils.Assertion.xs_assert

class FloatingReservationStation(bankNum:Int)(implicit p: Parameters) extends LazyModule with XSParam{
  private val wbNodeParam = WriteBackSinkParam(name = "Floating RS", sinkType = WriteBackSinkType.fpRs)
  private val rsParam = RsParam(name = "Floating RS", RsType.fp, 48, bankNum)
  val issueNode = new RsIssueNode(rsParam)
  val wakeupNode = new WriteBackSinkNode(wbNodeParam)

  lazy val module = new FloatingReservationStationImpl(this, rsParam)
}

class FloatingReservationStationImpl(outer:FloatingReservationStation, param:RsParam) extends LazyModuleImp(outer) with XSParam {
  require(param.bankNum == 4)
  require(param.entriesNum % param.bankNum == 0)
  private val issue = outer.issueNode.out.head._1 zip outer.issueNode.out.head._2
  println("Floating Reservation Issue Ports Config:")
  outer.issueNode.out.head._2.foreach(cfg => println(cfg))
  private val wbIn = outer.wakeupNode.in.head
  private val wakeup = wbIn._1.zip(wbIn._2)
  issue.foreach(elm => require(ExuType.fpTypes.contains(elm._2.exuType)))
  private val fmiscIssue = issue.filter(_._2.exuType == ExuType.fmisc)
  private val fmacIssue = issue.filter(_._2.exuType == ExuType.fmac)
  private val issuePortList = List(fmiscIssue, fmacIssue)
  require(fmiscIssue.nonEmpty && fmiscIssue.length <= param.bankNum && (param.bankNum % fmiscIssue.length) == 0)
  require(fmacIssue.nonEmpty && fmacIssue.length <= param.bankNum && (param.bankNum % fmacIssue.length) == 0)
  private val issueTypeNum = ExuType.fpTypes.length
  private val entriesNumPerBank = param.entriesNum / param.bankNum

  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val enq = Vec(param.bankNum, Flipped(DecoupledIO(new MicroOp)))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
  })
  io.enq.suggestName("new_enq")

  private val wakeupSignals = VecInit(wakeup.map(_._1).map(elm =>{
    val wkp = Wire(Valid(new WakeUpInfo))
    wkp.valid := elm.valid
    wkp.bits.pdest := elm.bits.uop.pdest
    wkp.bits.robPtr := elm.bits.uop.robIdx
    wkp.bits.lpv := 0.U.asTypeOf(wkp.bits.lpv)
    wkp
  }))
  private val rsBankSeq = Seq.tabulate(param.bankNum)( _ => {
    val mod = Module(new FloatingReservationBank(entriesNumPerBank, issueTypeNum, wakeup.length, loadUnitNum))
    mod.io.redirect := io.redirect
    mod.io.wakeup := wakeupSignals
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
    val rbIssUopPorts = rsBankSeq.map(_.io.issueUop(fuIdx))
    val rbIssMidStatePorts = rsBankSeq.map(_.io.issueMidResult(fuIdx))
    for((iss_elm, portIdx) <- iss.zipWithIndex) {
      prefix(s"${iss_elm._2.name}_${iss_elm._2.id}") {
        val issueBundle = Wire(Valid(new MicroOp))

        val bn = param.bankNum / iss.length
        val rbAddrPortsForThisPort = rbIssAddrPorts.slice(portIdx * bn, portIdx * bn + bn)
        val rbUopPortsForThisPort = rbIssUopPorts.slice(portIdx * bn, portIdx * bn + bn)
        val rbMidStatePortsForThisPort = rbIssMidStatePorts.slice(portIdx * bn, portIdx * bn + bn)
        val selectResps = sn.io.issueInfo(portIdx)

        issueBundle := DontCare
        issueBundle.valid := selectResps.valid
        issueBundle.bits := Mux1H(rbUopPortsForThisPort.map(elm => (elm.valid, elm.bits)))
        issueBundle.bits.robIdx := selectResps.bits.info.robPtr
        issueBundle.bits.ctrl.rfWen := selectResps.bits.info.rfWen
        issueBundle.bits.ctrl.fpWen := selectResps.bits.info.fpWen
        issueBundle.bits.pdest := selectResps.bits.info.pdest
        issueBundle.bits.ctrl.fuType := selectResps.bits.info.fuType
        issueBundle.bits.lpv := selectResps.bits.info.lpv

        val midState = Wire(Valid(new FMAMidResult))
        val issueValidDriverReg = RegInit(false.B)
        val issueBitsDriverReg = Reg(new MicroOp)
        val issueMidStateValidDriverReg = RegInit(false.B)
        val issueMidStateDataDriverReg = Reg(new FMAMidResult)
        val waitForAdd = RegInit(false.B)
        val issueAllow = !issueValidDriverReg || iss_elm._1.issue.fire
        when(issueAllow){
          issueValidDriverReg := issueBundle.valid
        }
        when(issueAllow && issueBundle.valid){
          issueBitsDriverReg := issueBundle.bits
          issueMidStateValidDriverReg := midState.valid
          issueMidStateDataDriverReg := midState.bits
          waitForAdd := selectResps.bits.info.fmaWaitAdd
        }

        iss_elm._1.issue.valid := issueValidDriverReg
        iss_elm._1.issue.bits.uop := issueBitsDriverReg
        iss_elm._1.fmaMidState.in.valid := issueMidStateValidDriverReg
        iss_elm._1.fmaMidState.in.bits := issueMidStateDataDriverReg
        iss_elm._1.fmaMidState.waitForAdd := waitForAdd

        val banksForThisPort = rsBankSeq.slice(portIdx * bn, portIdx * bn + bn)
        if(iss_elm._2.exuType == ExuType.fmac){
          val midStateWaitQueue = Module(new MidStateWaitQueue(selectResps.bits.bankIdxOH.getWidth, selectResps.bits.entryIdxOH.getWidth))
          midStateWaitQueue.io.redirect := io.redirect
          midStateWaitQueue.io.in.valid := iss_elm._1.issue.fire
          midStateWaitQueue.io.in.bits := RegEnable(selectResps.bits, issueAllow)
          banksForThisPort.zipWithIndex.foreach({case(u,idx) =>
            u.io.midResultReceived.valid := midStateWaitQueue.io.earlyWakeUp.valid && midStateWaitQueue.io.earlyWakeUp.bits.bankIdxOH(portIdx * bn + idx)
            u.io.midResultReceived.bits := midStateWaitQueue.io.earlyWakeUp.bits.entryIdxOH
            u.io.midResultEnq.valid := midStateWaitQueue.io.out.valid
            u.io.midResultEnq.bits.addrOH := midStateWaitQueue.io.out.bits.entryIdxOH
            u.io.midResultEnq.bits.data := iss_elm._1.fmaMidState.out.bits.midResult.asTypeOf(UInt(midState.getWidth.W))(midState.getWidth - 1, XLEN)
          })
          val isMidStateBypass = midStateWaitQueue.io.out.valid && selectResps.valid && midStateWaitQueue.io.out.bits.info.robPtr === selectResps.bits.info.robPtr
          val midStatePayload = Wire(Valid(new FMAMidResult))
          val bankSel = selectResps.bits.bankIdxOH.asBools.slice(portIdx * bn, portIdx * bn + bn)
          val bankMidResult = Mux1H(bankSel, rbMidStatePortsForThisPort)
          midStatePayload.valid := Cat(bankSel).orR && selectResps.bits.info.midResultReadEn
          midStatePayload.bits := Cat(bankMidResult, 0.U(XLEN.W)).asTypeOf(new FMAMidResult)
          val midStateBypass = Wire(Valid(new FMAMidResult))
          midStateBypass.valid := iss_elm._1.fmaMidState.out.valid
          midStateBypass.bits := iss_elm._1.fmaMidState.out.bits.midResult
          midState := Mux(isMidStateBypass, midStateBypass, midStatePayload)
          xs_assert(
            midStateWaitQueue.io.out.valid === iss_elm._1.fmaMidState.out.valid &&
              iss_elm._1.fmaMidState.out.bits.pdest === midStateWaitQueue.io.earlyWakeUp.bits.info.pdest,
            "MidState should be returned in a predictable latency with correct target!")
        } else {
          midState.valid := false.B
          midState.bits := DontCare
        }

        rbAddrPortsForThisPort.zipWithIndex.foreach({ case (rb, bidx) =>
          rb.valid := selectResps.bits.bankIdxOH(bidx + portIdx * bn) & issueAllow
          rb.bits := selectResps.bits.entryIdxOH
        })
      }
    }
  }
}
