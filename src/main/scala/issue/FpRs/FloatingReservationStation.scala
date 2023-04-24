package issue.FpRs
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import common.{FuType, MicroOp, Redirect, XSParam}
import exu.ExuType
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, ValName}
import freechips.rocketchip.macros.ValNameImpl
import fu.fpu.FMAMidResult
import issue._
import regfile.DecoupledPipeline
import writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}
import xs.utils.Assertion.xs_assert

import scala.collection.mutable

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
  issue.foreach(elm => elm._2.exuConfigs.foreach(elm0 => require(ExuType.fpTypes.contains(elm0.exuType))))

  private val fmacIssue = issue.filter(_._2.hasFmac)
  private val fdivIssue = issue.filter(_._2.hasFdiv)
  private val fmiscIssue = issue.filter(_._2.hasFmisc)

  require(fmacIssue.nonEmpty && fmacIssue.length <= param.bankNum && (param.bankNum % fmacIssue.length) == 0)
  require(fdivIssue.nonEmpty && fdivIssue.length <= param.bankNum && (param.bankNum % fdivIssue.length) == 0)
  require(fmiscIssue.nonEmpty && fmiscIssue.length <= param.bankNum && (param.bankNum % fmiscIssue.length) == 0)

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

  private val fmaIssuePortNum = issue.count(_._2.hasFmac)
  private val fdivIssuePortNum = issue.count(_._2.hasFdiv)
  private val fmiscIssuePortNum = issue.count(_._2.hasFmisc)
  private val fmaExuCfg = fmacIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.fmac).head
  private val fdivExuCfg = fdivIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.fdiv).head
  private val fmiscExuCfg = fmiscIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.fmisc).head

  private val fmacSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, fmaIssuePortNum, fmaExuCfg, Some(s"FloatingFmacSelectNetwork")))
  private val fdivSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, fdivIssuePortNum, fdivExuCfg, Some(s"FloatingFdivSelectNetwork")))
  private val fmiscSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, fmiscIssuePortNum, fmiscExuCfg, Some(s"FloatingFmiscSelectNetwork")))
  private val selectNetworkSeq = Seq(fmacSelectNetwork, fdivSelectNetwork, fmiscSelectNetwork)
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

  private val fmacSelectRespQueue = new mutable.Queue[SelectResp] ++ fmacSelectNetwork.io.issueInfo
  private val fdivSelectRespQueue = new mutable.Queue[SelectResp] ++ fdivSelectNetwork.io.issueInfo
  private val fmiscSelectRespQueue = new mutable.Queue[SelectResp] ++ fmiscSelectNetwork.io.issueInfo
  private val issueDataTupleList = issue.zipWithIndex.map({case((bundle, cfg),idx) =>
    if(cfg.isFmac){
      fmacIssue.find(_._2 ==)
    }

  })
}
