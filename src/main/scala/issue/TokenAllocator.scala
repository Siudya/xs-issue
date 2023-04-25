package issue
import chisel3._
import chisel3.util._
import common.{Redirect, RobPtr}
import xs.utils.Assertion.xs_assert
import xs.utils.PickOneLow
class TokenAllocatorEntry(pdestWidth:Int) extends Bundle{
  val robPtr = new RobPtr
  val pdest = UInt(pdestWidth.W)
}
class TokenAllocator(pdestWidth:Int, tokenNum:Int) extends Module{
  val io = IO(new Bundle{
    val alloc = Input(Valid(new TokenAllocatorEntry(pdestWidth)))
    val allow = Output(Bool())
    val release = Input(Valid(UInt(pdestWidth.W)))
    val redirect = Input(Valid(new Redirect))
  })
  private val valids = RegInit(VecInit(Seq.fill(tokenNum)(false.B)))
  private val payload = Reg(Vec(tokenNum, new TokenAllocatorEntry(pdestWidth)))

  private val emptyToken = PickOneLow(valids)
  io.allow := emptyToken.valid

  private val allocEnables = Mux(emptyToken.valid, emptyToken.bits, 0.U)
  valids.zip(payload).zip(allocEnables.asBools).foreach({
    case((v, d), en) =>
      when(v && (d.robPtr.needFlush(io.redirect) || (io.release.valid && d.pdest === io.release.bits))){
        v := false.B
      }.elsewhen(io.alloc.valid && en){
        v := true.B
      }

      when(io.alloc.valid && en){
        d := io.alloc.bits
      }
      xs_assert(Mux(en, !v, true.B))
  })
}
