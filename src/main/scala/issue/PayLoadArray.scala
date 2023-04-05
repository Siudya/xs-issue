package issue
import chisel3._
import chisel3.util._
import xs.utils.Assertion.xs_assert
class PayloadArrayReadIO(entryNum:Int) extends Bundle {
  val addr = Input(UInt(entryNum.W))
  val data = Output(new MicroOp)
}

class PayloadArrayWriteIO(entryNum:Int) extends Bundle {
  val en = Input(Bool())
  val addr   = Input(UInt(entryNum.W))
  val data   = Input(new MicroOp)
}

class PayloadArray(entryNum:Int, deqNum:Int, name:String) extends XSModule {
  val io = IO(new Bundle {
    val read = Vec(deqNum, new PayloadArrayReadIO(entryNum))
    val write = new PayloadArrayWriteIO(entryNum)
  })
  override val desiredName:String = name

  private val payload = Reg(Vec(entryNum, new MicroOp))

  // read ports
  io.read.foreach(r => {r.data := Mux1H(r.addr.asBools, payload)})

  // write ports
  for (idx <- 0 until entryNum) {
    val wen = io.write.en & io.write.addr(idx)
    val data = io.write.data
    when(wen){
      payload(idx) := data
    }
  }

  io.read.foreach(r => xs_assert(PopCount(r.addr) === 1.U))
  xs_assert(Mux(io.write.en, PopCount(io.write.addr) === 1.U, true.B))
}