package issue.MemRs

import issue._
import chisel3._
import chisel3.util._
import common.{FuType, MicroOp, Redirect}
import issue.MemRs.EntryState._
import issue.{EarlyWakeUpInfo, WakeUpInfo}

class MemoryReservationBank(entryNum:Int, stuNum:Int, lduNum:Int, mouNum:Int, wakeupWidth:Int) extends Module{
  private val issueWidth = stuNum * 2 + lduNum + mouNum
  val io = IO(new Bundle {
    val redirect = Input(Valid(new Redirect))

    val staSelectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))
    val stdSelectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))
    val lduSelectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))
    val mouSelectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))
    val allocateInfo = Output(UInt(entryNum.W))

    val enq = Input(Valid(new Bundle {
      val addrOH = UInt(entryNum.W)
      val data = new MicroOp
    }))

    val staIssue = Input(Vec(stuNum, Valid(UInt(entryNum.W))))
    val stdIssue = Input(Vec(stuNum, Valid(UInt(entryNum.W))))
    val lduIssue = Input(Vec(lduNum, Valid(UInt(entryNum.W))))
    val mouIssue = Input(Vec(mouNum, Valid(UInt(entryNum.W))))
    val staIssueUop = Output(Vec(stuNum, Valid(new MicroOp)))
    val stdIssueUop = Output(Vec(stuNum, Valid(new MicroOp)))
    val lduIssueUop = Output(Vec(lduNum, Valid(new MicroOp)))
    val mouIssueUop = Output(Vec(mouNum, Valid(new MicroOp)))

    val replay = Input(Vec(lduNum, Valid(new Bundle {
      val entryIdxOH = UInt(entryNum.W)
      val waitVal = UInt(5.W)
    })))

    val wakeup = Input(Vec(wakeupWidth, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Input(Vec(lduNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(lduNum, Bool()))
  })


  private val statusArray = Module(new MemoryStatusArray(entryNum, stuNum, lduNum, mouNum, wakeupWidth:Int))
  private val payloadArray = Module(new PayloadArray(new MicroOp, entryNum, issueWidth, "IntegerPayloadArray"))

  private def EnqToEntry(in: MicroOp): MemoryStatusArrayEntry = {
    val enqEntry = Wire(new MemoryStatusArrayEntry)
    enqEntry.psrc(0) := in.psrc(0)
    enqEntry.psrc(1) := in.psrc(1)
    enqEntry.srcType(0) := in.ctrl.srcType(0)
    enqEntry.srcType(1) := in.ctrl.srcType(1)
    enqEntry.srcState(0) := in.srcState(0)
    enqEntry.srcState(1) := in.srcState(1)
    enqEntry.pdest := in.pdest
    enqEntry.lpv.foreach(_.foreach(_ := 0.U))
    enqEntry.fuType := in.ctrl.fuType
    enqEntry.rfWen := in.ctrl.rfWen
    enqEntry.fpWen := in.ctrl.fpWen
    enqEntry.robIdx := in.robIdx
    enqEntry.staLoadState := Mux(in.ctrl.fuType === FuType.ldu, Mux(in.cf.loadWaitBit, s_wait_st, s_ready), Mux(in.ctrl.fuType === FuType.stu, s_ready, s_issued))
    enqEntry.stdMouState := Mux(in.ctrl.fuType === FuType.mou || in.ctrl.fuType === FuType.stu, s_ready, s_issued)
    enqEntry.waitTarget := in.cf.waitForRobIdx
    enqEntry
  }

  statusArray.io.redirect := io.redirect
  io.staSelectInfo := statusArray.io.staSelectInfo
  io.stdSelectInfo := statusArray.io.stdSelectInfo
  io.lduSelectInfo := statusArray.io.lduSelectInfo
  io.mouSelectInfo := statusArray.io.mouSelectInfo
  io.allocateInfo := statusArray.io.allocateInfo
  statusArray.io.enq.valid := io.enq.valid
  statusArray.io.enq.bits.addrOH := io.enq.bits.addrOH
  statusArray.io.enq.bits.data := EnqToEntry(io.enq.bits.data)
  statusArray.io.staIssue := io.staIssue
  statusArray.io.stdIssue := io.stdIssue
  statusArray.io.lduIssue := io.lduIssue
  statusArray.io.mouIssue := io.mouIssue
  statusArray.io.replay := io.replay
  statusArray.io.wakeup := io.wakeup
  statusArray.io.loadEarlyWakeup := io.loadEarlyWakeup
  statusArray.io.earlyWakeUpCancel := io.earlyWakeUpCancel

  payloadArray.io.write.en := io.enq.valid
  payloadArray.io.write.addr := io.enq.bits.addrOH
  payloadArray.io.write.data := io.enq.bits.data
  private val issueVec = io.staIssue ++ io.stdIssue ++ io.lduIssue ++ io.mouIssue
  private val issueUopVec = io.staIssueUop ++ io.stdIssueUop ++ io.lduIssueUop ++ io.mouIssueUop
  payloadArray.io.read.zip(issueVec).zip(issueUopVec).foreach({
    case((port, iAddr), iData) =>{
      port.addr := iAddr.bits
      iData.bits := port.data
      iData.valid := iAddr.valid
    }
  })
}

