package regfile
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import common.{ExuInput, Redirect, SrcType, XSBundle, XSModule}
import freechips.rocketchip.config.Parameters
import issue.RsIdx
import xs.utils.Assertion.xs_assert

class WritePort(dataWidth:Int) extends XSBundle {
  val addr = Input(UInt(MaxRegfileIdxWidth.W))
  val data = Input(UInt(dataWidth.W))
  val mask = Input(UInt((dataWidth / 8).W))
  val en = Input(Bool())
}

class ReadPort(dataWidth:Int) extends XSBundle {
  val addr = Input(UInt(MaxRegfileIdxWidth.W))
  val data = Output(UInt(dataWidth.W))
}

class GenericRegFile(entriesNum:Int, writeBackNum:Int, bypassNum:Int, readPortNum:Int, dataWidth:Int, moduleName:String, extraWriteNum:Int = 0)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val read = Vec(readPortNum, new ReadPort(dataWidth))
    val write = Vec(writeBackNum, new WritePort(dataWidth))
    val bypassWrite = Vec(bypassNum, new WritePort(dataWidth))
    val extraWrite = Vec(extraWriteNum, new WritePort(dataWidth))
  })
  override val desiredName = moduleName
  println(s"${moduleName} read ports: ${readPortNum}")

  private val bankNum = dataWidth / 8

  private val mems = List.tabulate(bankNum)(_ => Mem(entriesNum, UInt(XLEN.W)))
  (io.write ++ io.bypassWrite ++ io.extraWrite).foreach(w => {
    val writeData = Seq.tabulate(bankNum)(idx => w.data(idx * 8 + 7, idx * 8))
    val maskEn = w.mask.asBools
      when(w.en){
        mems.zip(writeData).zip(maskEn).foreach({case((m, d), en) =>
          when(en) {
            m(w.addr) := d
          }
        })
      }
  })

  io.read.foreach(r => {
    val memReadData = Cat(mems.map(_(r.addr)).reverse)
    if(bypassNum > 0) {
      val bypassHits = io.bypassWrite.map(w => w.en && w.addr === r.addr)
      val bypassData = Mux1H(bypassHits, io.bypassWrite.map(_.data))
      val bypassValid = bypassHits.reduce(_ | _)
      r.data := Mux(bypassValid, bypassData, memReadData)
    } else {
      r.data := memReadData
    }
  })
}