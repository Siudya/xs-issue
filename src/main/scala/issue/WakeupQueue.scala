package issue
import chisel3._
import chisel3.util._
import common.{Redirect, SrcType}
class WakeupQueue(latency:Int) extends Module{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val in = Input(Valid(new WakeUpInfo))
    val out = Output(Valid(new WakeUpInfo))
  })
  require(latency > 0)
  private def DelayInput(in:Valid[WakeUpInfo], l:Int): Valid[WakeUpInfo] = {
    val res = Wire(Valid(new WakeUpInfo))
    val realIn = if(l == 1) in else DelayInput(in, l - 1)
    val en = realIn.valid & realIn.bits.robPtr.needFlush(io.redirect)
    res.valid := RegNext(en, false.B)
    res.bits := RegEnable(in.bits, en)
    res
  }

  io.out := DelayInput(io.in, latency)
}

object WakeupQueue {
  def apply(in:DecoupledIO[SelectResp], latency:Int, redirect:Valid[Redirect]):Valid[WakeUpInfo] = {
    val res = Wire(Valid(new WakeUpInfo))
    if(latency > 0) {
      val wakeupQueue = Module(new WakeupQueue(latency))
      wakeupQueue.io.in.valid := in.fire
      wakeupQueue.io.in.bits.lpv := in.bits.info.lpv
      wakeupQueue.io.in.bits.robPtr := in.bits.info.robPtr
      wakeupQueue.io.in.bits.pdest := in.bits.info.pdest
      wakeupQueue.io.in.bits.destType := Mux(in.bits.info.fpWen, SrcType.fp, Mux(in.bits.info.rfWen, SrcType.reg, SrcType.default))
      wakeupQueue.io.redirect := redirect
      res := wakeupQueue.io.out
    } else {
      res.valid := in.fire
      res.bits.lpv := in.bits.info.lpv
      res.bits.robPtr := in.bits.info.robPtr
      res.bits.pdest := in.bits.info.pdest
      res.bits.destType := Mux(in.bits.info.fpWen, SrcType.fp, Mux(in.bits.info.rfWen, SrcType.reg, SrcType.default))
    }
    res
  }
}