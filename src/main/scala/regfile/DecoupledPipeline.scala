package regfile

import chisel3._
import chisel3.util._
import common.{ExuInput, MicroOp, Redirect}
import fu.fpu.FMAMidResult
import issue.IssueBundle
import xs.utils.Assertion.xs_assert
import xs.utils.{CircularQueuePtr, HasCircularQueuePtrHelper}
sealed class TwoEntryQueuePtr extends CircularQueuePtr[TwoEntryQueuePtr](entries = 2) with HasCircularQueuePtrHelper
sealed class PipelineEntry extends Bundle{
  val issue = new ExuInput
  val fmaMidStateIssue = Valid(new FMAMidResult)
  val fmaWaitForAdd = Bool()
}
class DecoupledPipeline(implementQueue:Boolean) extends Module{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val enq = Flipped(new IssueBundle)
    val deq = new IssueBundle
  })
  if(implementQueue) {
    val mem = Reg(Vec(2, new PipelineEntry))
    val enqPtr = RegInit(0.U.asTypeOf(new TwoEntryQueuePtr))
    val deqPtr = RegInit(0.U.asTypeOf(new TwoEntryQueuePtr))
    val full = enqPtr.value === deqPtr.value && enqPtr.flag =/= deqPtr.flag
    val empty = enqPtr.value === deqPtr.value && enqPtr.flag === deqPtr.flag
    val enqFire = io.enq.issue.fire
    val deqFire = io.deq.issue.fire
    io.enq.issue.ready := !full
    io.deq.issue.valid := !empty
    val outData = mem(deqPtr.value)
    io.deq.issue.bits := outData.issue
    io.deq.fmaMidState.waitForAdd := outData.fmaWaitForAdd
    io.deq.fmaMidState.in := outData.fmaMidStateIssue
    val inData = Wire(new PipelineEntry)
    inData.issue := io.enq.issue.bits
    inData.fmaWaitForAdd := io.enq.fmaMidState.waitForAdd
    inData.fmaMidStateIssue := io.enq.fmaMidState.in
    io.enq.fmaMidState.out := DontCare
    when(enqFire){
      mem(enqPtr.value) := inData
      enqPtr := enqPtr + 1.U
    }
    when(deqFire || (io.deq.issue.valid && io.deq.issue.bits.uop.robIdx.needFlush(io.redirect))) {
      when(full && mem(deqPtr.value + 1.U).issue.uop.robIdx.needFlush(io.redirect)){
        deqPtr := deqPtr + 2.U
      }.otherwise{
        deqPtr := deqPtr + 1.U
      }
    }
  } else {
    //ready should be true all the time
    io.enq.issue.ready := io.deq.issue.ready
    xs_assert(io.deq.issue.ready)
    io.deq.issue.valid := RegNext(io.enq.issue.valid, false.B)
    io.deq.issue.bits := RegEnable(io.enq.issue.bits, io.enq.issue.valid)
    io.deq.fmaMidState.in := RegEnable(io.enq.fmaMidState.in, io.enq.issue.valid)
    io.deq.fmaMidState.waitForAdd := RegEnable(io.enq.fmaMidState.waitForAdd, io.enq.issue.valid)
    io.enq.fmaMidState.out := DontCare
  }
}