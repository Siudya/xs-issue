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
/***************************************************************************************
  * Author: Liang Sen
  * E-mail: liangsen20z@ict.ac.cn
  * Date: 2023-03-31
  ****************************************************************************************/
package issue.IntRs
import chisel3._
import chisel3.util._
import issue._
class IntegerStatusArrayEntry extends BasicStatusArrayEntry(2, true,true){
  def toIssueInfo: Valid[SelectInfo] = {
    val src0Ready = srcState(0) === Fill(LpvLength, 1.U)
    val src1Ready = srcState(1) === Fill(LpvLength, 1.U)
    val src0ReadyLpv = PopCount(srcState(1)) === 1.U
    val src1ReadyLpv = PopCount(srcState(1)) === 1.U
    val checkSrcState = (src0Ready & src1Ready) | (src0Ready & src1ReadyLpv) | (src0ReadyLpv & src1Ready)
    val res = Wire(Valid(new SelectInfo))
    res.valid := checkSrcState
    res.bits.fuType := fuType
    res.bits.robPtr := robIdx
    res
  }
}

class BatchMux[T <: Data](gen: T, width:Int) extends Module{
  val io = IO(new Bundle{
    val sel = Input(UInt(width.W))
    val in0 = Input(Vec(width, gen))
    val in1 = Input(Vec(width, gen))
    val out = Output(Vec(width, gen))
  })
  for((((o, i0), i1), sel) <- io.out.zip(io.in0).zip(io.in1).zip(io.sel.asBools)){
    o := Mux(sel, i1, i0)
  }
}
class IntergerStatusArray(entryNum:Int, deqWidth:Int) extends Module{
  val io = IO(new Bundle{
    val issueInfo = Output(Vec(entryNum, Valid(new SelectInfo)))

    val enq = Input(Valid(new Bundle{
      val addrOH = UInt(entryNum.W)
      val data = new IntegerStatusArrayEntry
    }))
    val deq = Input(Vec(deqWidth, Valid(UInt(entryNum.W))))
  })

  private val statusArray = Reg(Vec(entryNum, new IntegerStatusArrayEntry))
  private val statusArrayValid = RegInit(0.U(entryNum.W))

  private val statusArrayNext = WireInit(statusArray)
  private val statusArrayValidNext = Wire(UInt(entryNum.W))
  statusArrayValidNext := statusArrayValid
  private val statusArrayEnable = WireInit(0.U(entryNum.W))

  private val statusArrayEnqData = Wire(Vec(entryNum, new IntegerStatusArrayEntry))
  private val statusArrayEnqValid = Wire(UInt(entryNum.W))
  private val statusArrayEnqEnable = Wire(UInt(entryNum.W))

  private val statusArrayFinalNext = Wire(Vec(entryNum, new IntegerStatusArrayEntry))
  private val statusArrayValidFinalNext = Wire(UInt(entryNum.W))
  private val statusArrayFinalEnable = Wire(UInt(entryNum.W))

  //Start of final update logic
  private val updateMux = Module(new BatchMux(Valid(new IntegerStatusArrayEntry), entryNum))
  updateMux.io.sel := Mux(io.enq.valid, io.enq.bits.addrOH, 0.U)
  updateMux.io.in0.zip(statusArrayNext).zip(statusArrayValidNext.asBools).foreach({
    case((i, d), v) =>
      i.valid := v
      i.bits := d
  })
  updateMux.io.in1.zip(statusArrayEnqData).zip(statusArrayEnqValid.asBools).foreach({
    case ((i, d), v) =>
      i.valid := v
      i.bits := d
  })
  updateMux.io.out.zip(statusArrayFinalNext).zip(statusArrayValidFinalNext.asBools).foreach({
    case ((o, d), v) =>
      v := o.valid
      d := o.bits
  })
  statusArrayFinalEnable := statusArrayEnable | statusArrayEnqEnable
  for(((((rd, rv), nd),nv),en) <- statusArray
    .zip(statusArrayValid.asBools)
    .zip(statusArrayFinalNext)
    .zip(statusArrayValidFinalNext.asBools)
    .zip(statusArrayFinalEnable.asBools)){
    when(en){
      rd := nd
      rv := nv
    }
  }
  //End of final update logic

  //Start of enqueue circuit description
  statusArrayEnqData.zip(statusArrayEnqValid.asBools).foreach(elm=> {
    elm._1 := io.enq.bits.data
    elm._2 := true.B
  })
  statusArrayEnqEnable := Mux(io.enq.valid, io.enq.bits.addrOH, 0.U)
  //End of enqueue circuit description

  //Start of select logic
  for(((issInfo, saEntry), saValid) <- io.issueInfo
    .zip(statusArray)
    .zip(statusArrayValid.asBools)){
    issInfo := Mux(saValid, saEntry.toIssueInfo, 0.U)
  }
  //End of select logic

  //TODO: Testing codes, should be removed.
  statusArrayEnable := io.deq.map(elm => Mux(elm.valid, elm.bits, 0.U)).reduce(_|_)
  private val entriesAffectByDeq = io.deq.map(elm => Mux(elm.valid, elm.bits, 0.U)).reduce(_|_)
  statusArray.zip(statusArrayValid.asBools).zipWithIndex.foreach({
    case((entry, entryValid),idx) =>
      when(entriesAffectByDeq(idx)){
        entryValid := false.B
        entry := 0.U
      }
  })
}
