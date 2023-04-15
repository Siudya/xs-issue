package issue
import chisel3._
import chisel3.util._
import common.{MicroOp, Redirect}
import xs.utils.Assertion.xs_assert
import xs.utils.{CircularQueuePtr, HasCircularQueuePtrHelper}
sealed class TwoEntryQueuePtr extends CircularQueuePtr[TwoEntryQueuePtr](entries = 2) with HasCircularQueuePtrHelper
class DecoupledPipeline(implementQueue:Boolean) extends Module{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val enq = Flipped(DecoupledIO(new MicroOp))
    val deq = DecoupledIO(new MicroOp)
  })
  if(implementQueue) {
    val mem = Reg(Vec(2, new MicroOp))
    val enqPtr = RegInit(0.U.asTypeOf(new TwoEntryQueuePtr))
    val deqPtr = RegInit(0.U.asTypeOf(new TwoEntryQueuePtr))
    val full = enqPtr.value === deqPtr.value && enqPtr.flag =/= deqPtr.flag
    val empty = enqPtr.value === deqPtr.value && enqPtr.flag === deqPtr.flag
    io.enq.ready := !full
    io.deq.valid := !empty
    io.deq.bits := mem(deqPtr.value)
    when(io.enq.fire){
      mem(enqPtr.value) := io.enq.bits
      enqPtr := enqPtr + 1.U
    }
    when(io.deq.fire || (io.deq.valid && io.deq.bits.robIdx.needFlush(io.redirect))) {
      when(full && mem(deqPtr.value + 1.U).robIdx.needFlush(io.redirect)){
        deqPtr := deqPtr + 2.U
      }.otherwise{
        deqPtr := deqPtr + 1.U
      }
    }
  } else {
    val validReg = RegNext(io.enq.valid, false.B)
    val dataReg = RegEnable(io.enq.bits, io.enq.valid)
    //ready should be true all the time
    io.enq.ready := io.deq.ready
    xs_assert(io.deq.ready)
    io.deq.valid := validReg
    io.deq.bits := dataReg
  }
}
object DecoupledPipeline{
  def apply(enq:DecoupledIO[MicroOp], redirect:Valid[Redirect]):DecoupledIO[MicroOp] = {
    val pipelineMod = Module(new DecoupledPipeline(true))
    pipelineMod.io.enq <> enq
    pipelineMod.io.redirect := redirect
    pipelineMod.io.deq
  }

  def apply(enq: DecoupledIO[MicroOp], redirect: Valid[Redirect], implementQueue:Boolean): DecoupledIO[MicroOp] = {
    val pipelineMod = Module(new DecoupledPipeline(implementQueue))
    pipelineMod.io.enq <> enq
    pipelineMod.io.redirect := redirect
    pipelineMod.io.deq
  }
}
