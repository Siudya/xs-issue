package regfile
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import common.{ExuInput, Redirect, SrcType, XSModule}
import freechips.rocketchip.config.Parameters
import issue.RsIdx
import xs.utils.Assertion.xs_assert

class GenericRegFile(entriesNum:Int, writeBackNum:Int, bypassNum:Int, readPortNum:Int, moduleName:String)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val read = Vec(readPortNum, new ReadPort(XLEN))
    val write = Vec(writeBackNum - bypassNum, new WritePort(XLEN))
    val bypassWrite = Vec(bypassNum, new WritePort(XLEN))
  })
  override val desiredName = moduleName

  private val mem = Mem(entriesNum, UInt(XLEN.W))
  (io.write ++ io.bypassWrite).foreach(w => {
      when(w.en){
        mem(w.addr) := w.data
      }
  })

  io.read.foreach(r => {
    val bypassHits = io.bypassWrite.map(w => w.en && w.addr === r.addr)
    val bypassData = Mux1H(bypassHits, io.bypassWrite.map(_.data))
    val bypassValid = bypassHits.reduce(_|_)
    r.data := Mux(bypassValid, bypassData, mem(r.addr))
  })
}