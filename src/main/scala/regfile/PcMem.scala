package regfile
import chisel3._
import chisel3.util._
import common.Ftq_RF_Components

class PcWritePort(addrWidth:Int) extends Bundle {
  val addr = Input(UInt(addrWidth.W))
  val data = Input(new Ftq_RF_Components)
  val en = Input(Bool())
}

class PcReadPort(addrWidth:Int) extends Bundle {
  val addr = Input(UInt(addrWidth.W))
  val data = Output(new Ftq_RF_Components)
}

class PcMem(numEntries:Int, numRead:Int, numWrite:Int) extends Module{
  val io = IO(new Bundle {
    val read = Vec(numRead, new PcReadPort(log2Ceil(numEntries)))
    val write = Vec(numWrite, new PcWritePort(log2Ceil(numEntries)))
  })

  private val mem = Mem(numEntries, new Ftq_RF_Components)
  io.write.foreach(w => {
    when(w.en){
      mem.write(w.addr, w.data)
    }
  })

  io.read.foreach(r => {
    val bypassHits = io.write.map(w => w.en && w.addr === r.addr)
    val bypassData = Mux1H(bypassHits, io.write.map(_.data))
    val bypassValid = bypassHits.reduce(_|_)
    r.data := Mux(bypassValid, bypassData, mem(r.addr))
  })
}
