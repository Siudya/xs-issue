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
import issue.RedirectLevel.flushItself
import issue._
import xs.utils.Assertion.xs_assert
class IntegerStatusArrayEntry extends BasicStatusArrayEntry(2, true){
  val issued = Bool()
  def toIssueInfo: Valid[SelectInfo] = {
    val readyToIssue = srcState(0) === SrcState.rdy & srcState(1) === SrcState.rdy & !issued
    val res = Wire(Valid(new SelectInfo))
    res.valid := readyToIssue
    res.bits.fuType := fuType
    res.bits.robPtr := robIdx
    res.bits.lpv := lpv(0) | lpv(1)
    res.bits.pdest := pdest
    res.bits.fpWen := fpWen
    res.bits.rfWen := rfWen
    res
  }
}

class IntegerStatusArrayEntryUpdateNetwork(issueWidth:Int, wakeupWidth:Int, loadUnitNum:Int) extends Module{
  val io = IO(new Bundle{
    val entry = Input(Valid(new IntegerStatusArrayEntry))
    val entryNext = Output(Valid(new IntegerStatusArrayEntry))
    val updateEnable = Output(Bool())
    val enq = Input(Valid(new IntegerStatusArrayEntry))
    val issue = Input(Vec(issueWidth, Bool()))
    val wakeup = Input(Vec(wakeupWidth, Valid(new WakeUpInfo(false))))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new WakeUpInfo(true))))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val redirect = Input(Valid(new Redirect))
  })

  io.entryNext := io.entry
  private val miscNext = WireInit(io.entry)
  private val miscUpdateEn = WireInit(false.B)
  private val enqNext = Wire(Valid(new IntegerStatusArrayEntry))
  private val enqUpdateEn = WireInit(false.B)
  private val redirectNext = WireInit(io.entry)

  //Start of wake up
  private val pregMatch = io.entry.bits.psrc.map(p => (io.wakeup ++ io.loadEarlyWakeup).map(elm => (elm.bits.pdest === p) && elm.valid))
  private val specPregMatch = io.entry.bits.psrc.map(p => io.loadEarlyWakeup.map(elm => elm.bits.pdest === p && elm.valid))
  for((((n, o), v), t) <- miscNext.bits.srcState zip io.entry.bits.srcState zip pregMatch zip io.entry.bits.srcType){
    when(Cat(v).orR){
      n := Mux(t === SrcType.reg, SrcState.rdy, o)
      miscUpdateEn := true.B
    }
  }
  private val miscUpdateEn0 = pregMatch.map(_.reduce(_|_)).reduce(_|_)
  //End of wake up

  //Start of issue and cancel
  private val shouldBeCancelled = io.earlyWakeUpCancel.zip(io.entry.bits.lpv).map({ case(c, l) => c & l(0)}).reduce(_|_)
  private val shouldBeIssued = Cat(io.issue).orR
  miscNext.bits.issued := Mux(shouldBeCancelled, false.B, Mux(shouldBeIssued, true.B, io.entry.bits.issued))
  private val miscUpdateEn1 = Cat(shouldBeCancelled, shouldBeIssued)
  //End of issue and cancel

  //Start of dequeue
  private val mayNeedReplay = io.entry.bits.lpv.map(_.orR).reduce(_|_)
  when(io.entry.bits.issued & !mayNeedReplay){
    miscNext.valid := false.B
    miscUpdateEn := true.B
  }
  //End of dequeue

  //Start of LPV shift
  for((ln,lo) <- miscNext.bits.lpv zip io.entry.bits.lpv){
    ln := Cat(0.U(1.W), lo(lo.getWidth-1,1))
  }
  private val miscUpdateEn2 = io.entry.bits.lpv.map(_.orR).reduce(_|_)
  //End of deq

  //Start of Enqueue
  enqNext.bits := io.enq.bits
  private val enqNotSuppressed = Mux(!io.redirect.valid, true.B,
    Mux(flushItself(io.redirect.bits.level), io.enq.bits.robIdx < io.redirect.bits.robIdx,
      io.enq.bits.robIdx <= io.redirect.bits.robIdx
    )
  )
  enqNext.valid := enqNotSuppressed & io.enq.valid
  enqUpdateEn := enqNext.valid
  //End of Enqueue

  //Start of redirect
  private val shouldBeFlushed = !enqNotSuppressed
  redirectNext.valid := false.B
  //End of redirect

  io.updateEnable := shouldBeFlushed | enqUpdateEn | miscUpdateEn2 | miscUpdateEn1 | miscUpdateEn0
  io.entryNext := Mux(enqUpdateEn, enqNext, Mux(shouldBeFlushed, redirectNext, miscNext))
}

class IntergerStatusArray(entryNum:Int, issueWidth:Int, wakeupWidth:Int, loadUnitNum:Int) extends Module{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))

    val issueInfo = Output(Vec(entryNum, Valid(new SelectInfo)))

    val enq = Input(Valid(new Bundle{
      val addrOH = UInt(entryNum.W)
      val data = new IntegerStatusArrayEntry
    }))

    val issue = Input(Vec(issueWidth, Valid(UInt(entryNum.W))))
    val wakeup = Input(Vec(wakeupWidth, Valid(new WakeUpInfo(false))))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new WakeUpInfo(true))))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
  })

  private val statusArray = Reg(Vec(entryNum, new IntegerStatusArrayEntry))
  private val statusArrayValid = RegInit(VecInit(Seq.fill(entryNum)(false.B)))

  //Start of select logic
  for(((issInfo, saEntry), saValid) <- io.issueInfo
    .zip(statusArray)
    .zip(statusArrayValid)){
    issInfo := Mux(saValid, saEntry.toIssueInfo, 0.U.asTypeOf(saEntry.toIssueInfo))
  }
  //End of select logic

  for(((v, d), idx) <- statusArrayValid
    .zip(statusArray)
    .zipWithIndex
  ){
    val updateNetwork = Module(new IntegerStatusArrayEntryUpdateNetwork(issueWidth, wakeupWidth, loadUnitNum))
    updateNetwork.io.entry.valid := v
    updateNetwork.io.entry.bits := d
    updateNetwork.io.enq.valid := io.enq.valid & io.enq.bits.addrOH(idx)
    updateNetwork.io.enq.bits := io.enq.bits.data
    updateNetwork.io.issue := VecInit(io.issue.map(i => i.valid & i.bits(idx)))
    updateNetwork.io.wakeup := io.wakeup
    updateNetwork.io.loadEarlyWakeup := io.loadEarlyWakeup
    updateNetwork.io.earlyWakeUpCancel := io.earlyWakeUpCancel
    updateNetwork.io.redirect := io.redirect

    val en = updateNetwork.io.updateEnable
    when(en) {
      v := updateNetwork.io.entryNext.valid
      d := updateNetwork.io.entryNext.bits
    }
  }
}
