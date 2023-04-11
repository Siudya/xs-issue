package issue
import chisel3._
import chisel3.util._
import common.{Redirect, WakeUpInfo}
class WakeupQueue(latency:Int) extends Module{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val in = Input(Valid(new WakeUpInfo))
    val out = Input(Valid(new WakeUpInfo))
  })
  private def DelayInput(in:Valid[WakeUpInfo], l:Int): Valid[WakeUpInfo] = {
    val res = Wire(Valid(new WakeUpInfo))
    val realIn = if(l == 1) in else DelayInput(in, l - 1)
    val en = realIn.valid & realIn.bits.robPtr.needFlush(io.redirect)
    res.valid := RegNext(en, false.B)
    res.bits := RegEnable(in.bits, en)
    res
  }

  if(latency == 0){
    io.out := io.in
  } else {
    io.out := DelayInput(io.in, latency)
  }
}
