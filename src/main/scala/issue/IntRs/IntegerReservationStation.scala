package issue.IntRs
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import common.{MicroOp, Redirect, XSParam}
import execute.exu.ExuType
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, ValName}
import freechips.rocketchip.macros.ValNameImpl
import execute.fu.fpu.FMAMidResult
import issue.FpRs.{DecoupledPipeline, FloatingReservationBank, MidStateWaitQueue}
import issue._
import writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}

import scala.collection.mutable

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
  println("Floating Reservation Issue Ports Config:")
  outer.issueNode.out.head._2.foreach(cfg => println(cfg))
  private val wbIn = outer.wakeupNode.in.head
  private val wakeup = wbIn._1.zip(wbIn._2)
  issue.foreach(elm => elm._2.exuConfigs.foreach(elm0 => require(ExuType.fpTypes.contains(elm0.exuType))))

  private val fmacIssue = issue.filter(_._2.hasFmac)
  private val fdivIssue = issue.filter(_._2.hasFdiv)
  private val fmiscIssue = issue.filter(_._2.hasFmisc)

  require(fmacIssue.nonEmpty && fmacIssue.length <= param.bankNum && (param.bankNum % fmacIssue.length) == 0)
  require(fdivIssue.nonEmpty && fdivIssue.length <= param.bankNum && (param.bankNum % fdivIssue.length) == 0)
  require(fmiscIssue.nonEmpty && fmiscIssue.length <= param.bankNum && (param.bankNum % fmiscIssue.length) == 0)

  private val issueWidth = issue.length
  private val entriesNumPerBank = param.entriesNum / param.bankNum

  val io = IO(new Bundle {
    val redirect = Input(Valid(new Redirect))
    val enq = Vec(param.bankNum, Flipped(DecoupledIO(new MicroOp)))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
  })
  io.enq.suggestName("new_enq")

  private val wakeupSignals = VecInit(wakeup.map(_._1).map(elm => {
    val wkp = Wire(Valid(new WakeUpInfo))
    wkp.valid := elm.valid
    wkp.bits.pdest := elm.bits.uop.pdest
    wkp.bits.robPtr := elm.bits.uop.robIdx
    wkp.bits.lpv := 0.U.asTypeOf(wkp.bits.lpv)
    wkp
  }))
  private val rsBankSeq = Seq.tabulate(param.bankNum)(_ => {
    val mod = Module(new FloatingReservationBank(entriesNumPerBank, issueWidth, wakeup.length, loadUnitNum))
    mod.io.redirect := io.redirect
    mod.io.wakeup := wakeupSignals
    mod.io.loadEarlyWakeup := io.loadEarlyWakeup
    mod.io.earlyWakeUpCancel := io.earlyWakeUpCancel
    mod
  })
  private val allocateNetwork = Module(new AllocateNetwork(param.bankNum, entriesNumPerBank, Some("IntegerAllocateNetwork")))

  private val fmaIssuePortNum = issue.count(_._2.hasFmac)
  private val fdivIssuePortNum = issue.count(_._2.hasFdiv)
  private val fmiscIssuePortNum = issue.count(_._2.hasFmisc)
  private val fmaExuCfg = fmacIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.fmac).head
  private val fdivExuCfg = fdivIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.fdiv).head
  private val fmiscExuCfg = fmiscIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.fmisc).head

  private val fmacSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, fmaIssuePortNum, fmaExuCfg, Some(s"FloatingFmacSelectNetwork")))
  private val fdivSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, fdivIssuePortNum, fdivExuCfg, Some(s"FloatingFdivSelectNetwork")))
  private val fmiscSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, fmiscIssuePortNum, fmiscExuCfg, Some(s"FloatingFmiscSelectNetwork")))
  fdivSelectNetwork.io.tokenRelease.get.zip(wakeup.filter(_._2.exuType == ExuType.fdiv).map(_._1)).foreach({
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits.uop.pdest
  })
  private val selectNetworkSeq = Seq(fmacSelectNetwork, fdivSelectNetwork, fmiscSelectNetwork)
  selectNetworkSeq.foreach(sn => {
    sn.io.selectInfo.zip(rsBankSeq).foreach({ case (sink, source) =>
      sink := source.io.selectInfo
    })
    sn.io.redirect := io.redirect
  })

  allocateNetwork.io.enqFromDispatch.zip(io.enq).foreach({ case (sink, source) =>
    sink.valid := source.valid
    sink.bits := source.bits
    source.ready := sink.ready
  })

  for (((fromAllocate, toAllocate), rsBank) <- allocateNetwork.io.enqToRs
    .zip(allocateNetwork.io.entriesValidBitVecList)
    .zip(rsBankSeq)) {
    toAllocate := rsBank.io.allocateInfo
    rsBank.io.enq.valid := fromAllocate.valid
    rsBank.io.enq.bits.data := fromAllocate.bits.uop
    rsBank.io.enq.bits.addrOH := fromAllocate.bits.addrOH
  }

  private val fmacSelectRespQueue = new mutable.Queue[DecoupledIO[SelectResp]] ++ fmacSelectNetwork.io.issueInfo
  private val fdivSelectRespQueue = new mutable.Queue[DecoupledIO[SelectResp]] ++ fdivSelectNetwork.io.issueInfo
  private val fmiscSelectRespQueue = new mutable.Queue[DecoupledIO[SelectResp]] ++ fmiscSelectNetwork.io.issueInfo

  private val fmacComplexIssuePorts = issue.filter(_._2.isFmac)
  private val fmaDivComplexIssuePorts = issue.filter(_._2.isFmaDiv)
  private val fmaMiscComplexIssuePorts = issue.filter(_._2.isFmaMisc)
  private val issuePortsSeq = Seq(fmacComplexIssuePorts, fmaDivComplexIssuePorts, fmaMiscComplexIssuePorts)
  private val bankNumForOneFmaPort = param.bankNum / fmaIssuePortNum
  for ((issSeq, seqIdx) <- issuePortsSeq.zipWithIndex) {
    val bankNumForEach = param.bankNum / issSeq.length

    for ((iss, issIdx) <- issSeq.zipWithIndex) {
      val issueDriver = Module(new DecoupledPipeline(true, param.bankNum, entriesNumPerBank))
      issueDriver.io.redirect := io.redirect

      val midStateWaitQueue = Module(new MidStateWaitQueue(2, param.bankNum, entriesNumPerBank))
      midStateWaitQueue.io.redirect := io.redirect

      val issuePortIdx = issuePortsSeq.take(seqIdx).map(_.length).sum + issIdx

      val selectInfoSeq = if (iss._2.isFmac) {
        Seq(fmacSelectRespQueue.dequeue())
      } else if (iss._2.isFmaDiv) {
        Seq(fmacSelectRespQueue.dequeue(), fdivSelectRespQueue.dequeue())
      } else {
        Seq(fmacSelectRespQueue.dequeue(), fmiscSelectRespQueue.dequeue())
      }

      val finalSelectInfo = if (iss._2.isFmac) {
        selectInfoSeq.head
      } else {
        val selectRespArbiter = Module(new Arbiter(selectInfoSeq.head.bits, 2))
        selectRespArbiter.io.in(0) := selectInfoSeq.head
        selectRespArbiter.io.in(0) := selectInfoSeq.last
        selectRespArbiter.io.out
      }

      val rsBankRen = Mux(issueDriver.io.enq.fire, finalSelectInfo.bits.bankIdxOH, 0.U)
      rsBankSeq.zip(rsBankRen.asBools).foreach({ case (rb, ren) =>
        rb.io.issueAddr(issuePortIdx).valid := ren
        rb.io.issueAddr(issuePortIdx).bits := finalSelectInfo.bits.entryIdxOH
      })

      val issueBundle = Wire(Valid(new MicroOp))
      issueBundle.valid := finalSelectInfo.valid
      issueBundle.bits := Mux1H(rsBankRen, rsBankSeq.map(_.io.issueUop(bankNumForEach).bits))
      issueBundle.bits.robIdx := finalSelectInfo.bits.info.robPtr
      issueBundle.bits.ctrl.rfWen := finalSelectInfo.bits.info.rfWen
      issueBundle.bits.ctrl.fpWen := finalSelectInfo.bits.info.fpWen
      issueBundle.bits.pdest := finalSelectInfo.bits.info.pdest
      issueBundle.bits.ctrl.fuType := finalSelectInfo.bits.info.fuType
      issueBundle.bits.lpv := finalSelectInfo.bits.info.lpv

      val midResultFromPayload = Wire(new FMAMidResult)
      val midResultFromPayloadHi = Mux1H(rsBankRen, rsBankSeq.map(_.io.issueMidResult(bankNumForEach)))
      val midResultFromPayloadLo = 0.U(XLEN - 1, 0)
      midResultFromPayload := Cat(midResultFromPayloadHi, midResultFromPayloadLo)

      val midResultFromBypass = Wire(new FMAMidResult)
      midResultFromBypass := iss._1.fmaMidState.out.bits.midResult

      val midResult = Wire(new FMAMidResult)

      finalSelectInfo.ready := issueDriver.io.enq.ready
      issueDriver.io.enq.valid := issueBundle.valid
      issueDriver.io.enq.bits.uop := issueBundle.bits
      issueDriver.io.enq.bits.fmaMidStateIssue.valid := finalSelectInfo.bits.info.midResultReadEn
      issueDriver.io.enq.bits.fmaMidStateIssue.bits := midResult
      issueDriver.io.enq.bits.fmaWaitForAdd := finalSelectInfo.bits.info.fmaWaitAdd

      iss._1.issue.valid := issueDriver.io.deq.valid
      iss._1.issue.bits.uop := issueDriver.io.deq.bits.uop
      iss._1.issue.bits.src := DontCare
      iss._1.fmaMidState.in := issueDriver.io.deq.bits.fmaMidStateIssue
      iss._1.fmaMidState.waitForAdd := issueDriver.io.deq.bits.fmaWaitForAdd
      issueDriver.io.deq.ready := iss._1.issue.ready

      val midStateWaitQueueInValidReg = RegInit(false.B)
      val midStateWaitQueueInDataReg = Reg(new SelectResp(param.bankNum, entriesNumPerBank))
      when(issueDriver.io.deq.fire && !issueDriver.io.deq.bits.uop.robIdx.needFlush(io.redirect)) {
        midStateWaitQueueInValidReg := issueDriver.io.deq.bits.fmaWaitForAdd
      }
      when(issueDriver.io.deq.fire && issueDriver.io.deq.bits.fmaWaitForAdd && !issueDriver.io.deq.bits.uop.robIdx.needFlush(io.redirect)) {
        midStateWaitQueueInDataReg := issueDriver.io.deq.bits.fmaWaitForAdd
        midStateWaitQueueInDataReg.bankIdxOH := issueDriver.io.deq.bits.bankIdxOH
        midStateWaitQueueInDataReg.entryIdxOH := issueDriver.io.deq.bits.entryIdxOH
        midStateWaitQueueInDataReg.info := DontCare
        midStateWaitQueueInDataReg.info.robPtr := issueDriver.io.deq.bits.uop.robIdx
        midStateWaitQueueInDataReg.info.pdest := issueDriver.io.deq.bits.uop.pdest
      }
      midStateWaitQueue.io.in.valid := midStateWaitQueueInValidReg && iss._1.fuInFire && !midStateWaitQueueInDataReg.info.robPtr.needFlush(io.redirect)
      midStateWaitQueue.io.in.bits := midStateWaitQueueInDataReg

      val bankForThisWaitQueue = rsBankSeq.slice(issuePortIdx * bankNumForOneFmaPort, issuePortIdx * bankNumForOneFmaPort + bankNumForOneFmaPort)
      val bankEn = Mux(midStateWaitQueue.io.earlyWakeUp.valid, midStateWaitQueue.io.out.bits.bankIdxOH, 0.U)
        .asBools.slice(issuePortIdx * bankNumForOneFmaPort, issuePortIdx * bankNumForOneFmaPort + bankNumForOneFmaPort)
      bankForThisWaitQueue.zip(bankEn).foreach({
        case (rb, en) =>
          rb.io.midResultReceived.valid := en
          rb.io.midResultReceived.bits := midStateWaitQueue.io.earlyWakeUp.bits.entryIdxOH
      })

      val midStateShouldBypass =
        iss._1.fmaMidState.out.valid &&
          midStateWaitQueue.io.out.valid &&
          midStateWaitQueue.io.out.bits.info.pdest === iss._1.fmaMidState.out.bits.pdest

      midResult := Mux(midStateShouldBypass, midResultFromBypass, midResultFromPayload)
    }
  }
}
