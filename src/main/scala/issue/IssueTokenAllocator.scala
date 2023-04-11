package issue
import chisel3._
import chisel3.util._
import common.{Redirect, RobPtr}
import xs.utils.Assertion.xs_assert

class IssueTokenAllocator(tokenNum:Int) extends Module{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val request = Input(Valid(new RobPtr))
    val grant = Output(Valid(UInt(tokenNum.W)))
    val release = Input(UInt(tokenNum.W))
  })

  private val table = RegInit(VecInit(Seq.fill(tokenNum)(0.U.asTypeOf(Valid(new RobPtr)))))
  private val emptyIdx = EntryIdxGenerator(table.map(!_.valid))
  table.zip(emptyIdx.bits.asBools).zip(io.release.asBools).foreach({
    case((entry, hit), release) =>
      val redirectCond = entry.valid && entry.bits.needFlush(io.redirect)
      val releaseCond = entry.valid && release
      xs_assert(Mux(release, entry.valid, true.B))
      val allocateCond = emptyIdx.valid && hit && io.request.valid
      when(redirectCond || releaseCond){
        entry.valid := false.B
      }.elsewhen(allocateCond){
        entry.valid := true.B
        entry.bits := io.request.bits.robPtr
      }
  })
  io.grant.valid := io.request.valid & emptyIdx.valid
  io.grant.bits := emptyIdx.bits

  xs_assert(Mux(emptyIdx.valid, Mux1H(emptyIdx.bits.asBools, table).valid === 0.U, true.B))
}
