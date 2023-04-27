package regfile
import chisel3._
import chisel3.util._
import common.XSBundle

class WritePort(dataWidth:Int) extends XSBundle {
  val addr = UInt(MaxRegfileIdxWidth.W)
  val data = UInt(dataWidth.W)
  val en = Bool()
}

class ReadPort(dataWidth:Int) extends XSBundle {
  val addr = UInt(MaxRegfileIdxWidth.W)
  val data = UInt(dataWidth.W)
}