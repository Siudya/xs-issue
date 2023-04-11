package issue
import chisel3._
import chisel3.util._
import xs.utils.Assertion.xs_assert
import xs.utils.ParallelOperation
class EntryIdxGenerator(entryNum:Int) extends Module {
  private val idxWidth = entryNum
  val io = IO(new Bundle {
    val entriesValidBitVec = Input(UInt(entryNum.W))
    val entryIndexOH = Valid(UInt(idxWidth.W))
  })
  private val idxOHList = Seq.tabulate(entryNum)(idx => (1 << idx).U(idxWidth.W))
  private val candidates = io.entriesValidBitVec.asBools zip idxOHList
  private def validMux(a:(Bool,UInt), b:(Bool,UInt)) : (Bool,UInt) = {
    val resData = Mux(a._1, a._2, b._2)
    val valid = a._1 | b._1
    (valid, resData)
  }
  private val res = ParallelOperation(candidates, validMux)
  io.entryIndexOH.valid := res._1
  io.entryIndexOH.bits := res._2

  xs_assert(Mux(io.entryIndexOH.valid, (io.entryIndexOH.bits & io.entriesValidBitVec).orR, true.B))
}

object EntryIdxGenerator{
  def apply(in:UInt):Valid[UInt] = {
    val entryIdxGenerator = Module(new EntryIdxGenerator(in.getWidth))
    entryIdxGenerator.io.entriesValidBitVec := in
    entryIdxGenerator.io.entryIndexOH
  }
  def apply(in: Seq[Bool]): Valid[UInt] = {
    val entryIdxGenerator = Module(new EntryIdxGenerator(in.length))
    entryIdxGenerator.io.entriesValidBitVec := Cat(in.reverse)
    entryIdxGenerator.io.entryIndexOH
  }
}