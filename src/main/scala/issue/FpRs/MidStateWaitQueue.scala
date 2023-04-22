package issue.FpRs

import chisel3._
import chisel3.util._
import common.Redirect
import issue.SelectResp

class MidStateWaitQueue(bankIdxWidth:Int, entryIdxWidth:Int) extends Module{
  val io = IO(new Bundle {
    val redirect = Input(Valid(new Redirect))
    val in = Input(Valid(new SelectResp(bankIdxWidth, entryIdxWidth)))
    val out = Output(Valid(new SelectResp( bankIdxWidth, entryIdxWidth)))
    val earlyWakeUp = Output(Valid(new SelectResp(bankIdxWidth, entryIdxWidth)))
  })
  private val latency = 3
  private val validRegs = RegInit(VecInit(Seq.fill(latency)(false.B)))
  private val dataRegs = List.fill(latency)(Reg(new SelectResp(bankIdxWidth, entryIdxWidth)))
  for(idx <- (io.in.valid +: validRegs).indices){
    if(idx == 1){
      validRegs(idx) := validRegs(idx - 1)
      when(validRegs(idx - 1)) {
        dataRegs(idx) := dataRegs(idx - 1)
      }
    } else if(idx > 1){
      val valid = validRegs(idx - 1) & !dataRegs(idx - 1).info.robPtr.needFlush(io.redirect)
      validRegs(idx) := valid
      when(valid) {
        dataRegs(idx) := dataRegs(idx - 1)
      }
    }
  }
  io.out.valid := validRegs.last
  io.out.bits := dataRegs.last
  io.earlyWakeUp.valid := validRegs.takeRight(2).head
  io.earlyWakeUp.bits := dataRegs.takeRight(2).head
}
