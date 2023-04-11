/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import common._
class FunctionUnitIO(val len: Int)(implicit p: Parameters) extends XSBundle {
  val in = Flipped(ValidIO(new ExuInput(len)))
  val out = ValidIO(new ExuOutput)
  val redirectIn = Flipped(ValidIO(new Redirect))
}

abstract class FunctionUnit(len: Int = 64)(implicit p: Parameters) extends XSModule {
  val io = IO(new FunctionUnitIO(len))
}

abstract class FUWithRedirect(len: Int = 64)(implicit p: Parameters) extends FunctionUnit(len: Int) with HasRedirectOut

trait HasRedirectOut { this: XSModule =>
  val redirectOutValid = IO(Output(Bool()))
  val redirectOut = IO(Output(new Redirect))
}

trait HasPipelineReg {
  this: FunctionUnit =>
  def latency: Int

  require(latency > 0)

  val validVec = io.in.valid +: Seq.fill(latency)(RegInit(false.B))
  val uopVec = io.in.bits.uop +: Seq.fill(latency)(Reg(new MicroOp))


  // if flush(0), valid 0 will not given, so set flushVec(0) to false.B
  val flushVec = validVec.zip(uopVec).map(x => x._1 && x._2.robIdx.needFlush(io.redirectIn))

  for (i <- 1 to latency) {
    when(validVec(i - 1) && !flushVec(i - 1)){
      validVec(i) := validVec(i - 1)
      uopVec(i) := uopVec(i - 1)
    }.elsewhen(flushVec(i)){
      validVec(i) := false.B
    }
  }

  io.out.valid := validVec.takeRight(2).head
  io.out.bits.uop := uopVec.takeRight(2).head

  def regEnable(i: Int): Bool = validVec(i - 1) && !flushVec(i - 1)

  def PipelineReg[TT <: Data](i: Int)(next: TT) = RegEnable(
    next,
    enable = regEnable(i)
  )

  def S1Reg[TT <: Data](next: TT): TT = PipelineReg[TT](1)(next)

  def S2Reg[TT <: Data](next: TT): TT = PipelineReg[TT](2)(next)

  def S3Reg[TT <: Data](next: TT): TT = PipelineReg[TT](3)(next)

  def S4Reg[TT <: Data](next: TT): TT = PipelineReg[TT](4)(next)

  def S5Reg[TT <: Data](next: TT): TT = PipelineReg[TT](5)(next)
}
