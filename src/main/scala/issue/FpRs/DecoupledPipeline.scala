package issue.FpRs

import chisel3._
import chisel3.util._
import common.{ExuInput, MicroOp, Redirect}
import fu.fpu.FMAMidResult
import issue.IssueBundle
import xs.utils.Assertion.xs_assert
import xs.utils.{CircularQueuePtr, HasCircularQueuePtrHelper}
sealed class TwoEntryQueuePtr extends CircularQueuePtr[TwoEntryQueuePtr](entries = 2) with HasCircularQueuePtrHelper
sealed class PipelineEntry(bankIdxWidth:Int, entryIdxWidth:Int) extends Bundle{
  val uop = new MicroOp
  val fmaMidStateIssue = Valid(new FMAMidResult)
  val fmaWaitForAdd = Bool()
  val bankIdxOH = UInt(bankIdxWidth.W)
  val entryIdxOH = UInt(entryIdxWidth.W)
}
class DecoupledPipeline(implementQueue:Boolean, bankIdxWidth:Int, entryIdxWidth:Int) extends Module{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val enq = Flipped(DecoupledIO(new PipelineEntry(bankIdxWidth, entryIdxWidth)))
    val deq = DecoupledIO(new PipelineEntry(bankIdxWidth, entryIdxWidth))
  })
  if(implementQueue) {
    val mem = Reg(Vec(2, new PipelineEntry(bankIdxWidth, entryIdxWidth)))
    val enqPtr = RegInit(0.U.asTypeOf(new TwoEntryQueuePtr))
    val deqPtr = RegInit(0.U.asTypeOf(new TwoEntryQueuePtr))
    val full = enqPtr.value === deqPtr.value && enqPtr.flag =/= deqPtr.flag
    val empty = enqPtr.value === deqPtr.value && enqPtr.flag === deqPtr.flag
    val enqFire = io.enq.fire
    val deqFire = io.deq.fire
    io.enq.ready := !full
    io.deq.valid := !empty
    io.deq.bits := mem(deqPtr.value)
    when(enqFire){
      mem(enqPtr.value) := io.enq.bits
      enqPtr := enqPtr + 1.U
    }
    when(deqFire || (io.deq.valid && io.deq.bits.uop.robIdx.needFlush(io.redirect))) {
      when(full && mem(deqPtr.value + 1.U).uop.robIdx.needFlush(io.redirect)){
        deqPtr := deqPtr + 2.U
      }.otherwise{
        deqPtr := deqPtr + 1.U
      }
    }
  } else {
    //ready should be true all the time
    io.enq.ready := io.deq.ready
    xs_assert(io.deq.ready)
    io.deq.valid := RegNext(io.enq.valid, false.B)
    io.deq.bits := RegEnable(io.enq.bits, io.enq.valid)
  }
}