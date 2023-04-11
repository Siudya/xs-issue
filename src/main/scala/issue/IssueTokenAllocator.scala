package issue
import chisel3._
import chisel3.util._
import common.{Redirect, RobPtr}
import xs.utils.Assertion.xs_assert
class TokenAllocatorEntry(PhyRegFileIdxWidth:Int) extends Bundle{
  val pdest = UInt(PhyRegFileIdxWidth.W)
  val robPtr = new RobPtr
}
class IssueTokenAllocator(PhyRegFileIdxWidth:Int, tokenNum:Int) extends Module{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val request = Input(Valid(new TokenAllocatorEntry(PhyRegFileIdxWidth)))
    val grant = Output(Bool())
    val release = Input(Valid(UInt(PhyRegFileIdxWidth.W)))
  })

  private val table = RegInit(VecInit(Seq.fill(tokenNum)(0.U.asTypeOf(Valid(new TokenAllocatorEntry(PhyRegFileIdxWidth))))))
  io.grant := io.request.valid & Cat(table.map(~_.valid)).orR

  private val emptyIdx = EntryIdxGenerator(table.map(!_.valid))
  table.zip(emptyIdx.bits.asBools).foreach({
    case(entry, hit) =>
      val redirectCond = entry.valid && entry.bits.robPtr.needFlush(io.redirect)
      val releaseCond = entry.valid && io.release.valid && io.release.bits === entry.bits.pdest
      val allocateCond = emptyIdx.valid && hit && io.request.valid
      when(redirectCond || releaseCond){
        entry.valid := false.B
      }.elsewhen(allocateCond){
        entry.valid := true.B
        entry.bits.pdest := io.request.bits.pdest
        entry.bits.robPtr := io.request.bits.robPtr
      }
  })
  xs_assert(Mux(emptyIdx.valid, Mux1H(emptyIdx.bits.asBools, table).valid === 0.U, true.B))
}
