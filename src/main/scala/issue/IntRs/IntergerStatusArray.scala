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
import chisel3.experimental.ChiselAnnotation
import chisel3.util._
import common.{Redirect, SrcState, SrcType, XSModule}
import issue._
import xs.utils.Assertion.xs_assert
import xs.utils.LogicShiftRight
import firrtl.passes.InlineAnnotation
import issue.IntRs.EntryState._
protected[IntRs] object EntryState{
  def s_ready:UInt = 0.U
  def s_wait_cancel: UInt = 1.U
  def s_issued: UInt = 2.U
  def apply() = UInt(2.W)
}
class IntegerIssueInfoGenerator extends XSModule{
  val io = IO(new Bundle{
    val in = Input(Valid(new IntegerStatusArrayEntry))
    val out = Output(Valid(new SelectInfo))
    val redirect = Input(Valid(new Redirect))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
  })
  private val iv = io.in.valid
  private val ib = io.in.bits
  private val shouldBeFlushed = ib.robIdx.needFlush(io.redirect)
  private val shouldBeCanceled = ib.lpv.map(l => l.zip(io.earlyWakeUpCancel).map({ case (li, c) => li(1) && c }).reduce(_ || _)).reduce(_ || _)
  private val readyToIssue = ib.srcState(0) === SrcState.rdy & ib.srcState(1) === SrcState.rdy && ib.state === s_ready
  io.out.valid := readyToIssue && iv && !shouldBeFlushed && !shouldBeCanceled
  io.out.bits.fuType := ib.fuType
  io.out.bits.robPtr := ib.robIdx
  io.out.bits.pdest := ib.pdest
  io.out.bits.fpWen := ib.fpWen
  io.out.bits.rfWen := ib.rfWen
  io.out.bits.lpv.zip(ib.lpv.transpose).foreach({case(o, i) => o := i.reduce(_|_)})
  io.out.bits.fmaWaitAdd := false.B
  io.out.bits.midResultReadEn := false.B

  chisel3.experimental.annotate(new ChiselAnnotation {
    def toFirrtl = InlineAnnotation(toNamed)
  })
}
class IntegerStatusArrayEntry extends BasicStatusArrayEntry(2, true){
  val state = EntryState()
}

class IntegerStatusArrayEntryUpdateNetwork(issueWidth:Int, wakeupWidth:Int) extends XSModule{
  val io = IO(new Bundle{
    val entry = Input(Valid(new IntegerStatusArrayEntry))
    val entryNext = Output(Valid(new IntegerStatusArrayEntry))
    val updateEnable = Output(Bool())
    val enq = Input(Valid(new IntegerStatusArrayEntry))
    val issue = Input(Vec(issueWidth, Bool()))
    val wakeup = Input(Vec(wakeupWidth, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val redirect = Input(Valid(new Redirect))
  })

  io.entryNext := io.entry
  private val miscNext = WireInit(io.entry)
  private val enqNext = Wire(Valid(new IntegerStatusArrayEntry))
  private val enqUpdateEn = WireInit(false.B)

  //Start of wake up
  private val pregMatch = io.entry.bits.psrc
    .zip(io.entry.bits.srcType)
    .map(p => (io.wakeup ++ io.loadEarlyWakeup).map(elm => (elm.bits.pdest === p._1) && elm.valid && p._2 === elm.bits.destType))
  for((n, v) <- miscNext.bits.srcState zip pregMatch){
    val shouldUpdateSrcState = Cat(v).orR
    when(shouldUpdateSrcState){
      n := SrcState.rdy
    }
  }
  private val miscUpdateEnWakeUp = pregMatch.map(_.reduce(_|_)).reduce(_|_)
  //End of wake up

  //Start of issue and cancel
  private val srcShouldBeCancelled = io.entry.bits.lpv.map(l => io.earlyWakeUpCancel.zip(l).map({ case(c, li) => li(0) & c}).reduce(_|_))
  private val shouldBeIssued = Cat(io.issue).orR
  private val shouldBeCancelled = srcShouldBeCancelled.reduce(_|_)
  private val mayNeedReplay = io.entry.bits.lpv.map(_.map(_.orR).reduce(_|_)).reduce(_|_)
  private val state = io.entry.bits.state
  private val stateNext = io.entryNext.bits.state

  switch(state){
    is(s_ready){
      when(shouldBeIssued){
        stateNext := Mux(mayNeedReplay, s_wait_cancel, s_issued)
      }
    }
    is(s_wait_cancel){
      when(!mayNeedReplay){
        stateNext := s_issued
      }.elsewhen(shouldBeCancelled){
        stateNext := s_ready
      }
    }
  }
  xs_assert(Mux(io.entry.valid, state === s_ready || state === s_wait_cancel || state === s_issued, true.B))

  srcShouldBeCancelled.zip(miscNext.bits.srcState).foreach{case(en, state) => when(en){state := SrcState.busy}}

  private val miscUpdateEnCancelOrIssue = shouldBeIssued || shouldBeCancelled || mayNeedReplay

  //End of issue and cancel

  //Start of dequeue and redirect
  private val shouldBeFlushed = io.entry.valid & io.entry.bits.robIdx.needFlush(io.redirect)
  private val miscUpdateEnDequeueOrRedirect = io.entry.bits.state === s_issued || shouldBeFlushed
  when(miscUpdateEnDequeueOrRedirect) {
    miscNext.valid := false.B
  }
  //End of dequeue and redirect

  //Start of LPV behaviors
  private val lpvModified = Wire(Vec(miscNext.bits.lpv.length, Vec(miscNext.bits.lpv.head.length, Bool())))
  lpvModified.foreach(_.foreach(_ := false.B))
  for(((((newLpvs, oldLpvs),mod), st), psrc) <- miscNext.bits.lpv.zip(io.entry.bits.lpv).zip(lpvModified).zip(io.entry.bits.srcType).zip(io.entry.bits.psrc)){
    for(((((nl, ol), ewkp), m),idx) <- newLpvs.zip(oldLpvs).zip(io.loadEarlyWakeup).zip(mod).zipWithIndex){
      val earlyWakeUpHit = ewkp.valid && ewkp.bits.pdest === psrc && st === ewkp.bits.destType
      val regularWakeupHits = io.wakeup.map(wkp => wkp.valid && wkp.bits.pdest === psrc && st === wkp.bits.destType)
      val regularWakeupLpv = io.wakeup.map(wkp => wkp.bits.lpv(idx))
      val lpvUpdateHitsVec = regularWakeupHits :+ earlyWakeUpHit
      val lpvUpdateDataVec = regularWakeupLpv :+ ewkp.bits.lpv
      val wakeupLpvValid = lpvUpdateHitsVec.reduce(_|_)
      val wakeupLpvSelected = Mux1H(lpvUpdateHitsVec, lpvUpdateDataVec)
      nl := Mux(wakeupLpvValid, wakeupLpvSelected, LogicShiftRight(ol,1))
      m := wakeupLpvValid | ol.orR
      xs_assert(Mux(wakeupLpvValid, !(ol.orR), true.B))
    }
  }
  private val miscUpdateEnLpvUpdate = lpvModified.map(_.reduce(_|_)).reduce(_|_)
  //End of LPV behaviors

  //Start of Enqueue
  enqNext.bits := io.enq.bits
  private val enqShouldBeSuppressed = io.enq.bits.robIdx.needFlush(io.redirect)
  enqNext.valid := !enqShouldBeSuppressed && io.enq.valid
  enqUpdateEn := enqNext.valid
  //End of Enqueue

  io.updateEnable := Mux(io.entry.valid, miscUpdateEnWakeUp | miscUpdateEnCancelOrIssue | miscUpdateEnDequeueOrRedirect | miscUpdateEnLpvUpdate, enqUpdateEn)
  io.entryNext := Mux(enqUpdateEn, enqNext, miscNext)

  chisel3.experimental.annotate(new ChiselAnnotation {
    def toFirrtl = InlineAnnotation(toNamed)
  })
}

class IntegerStatusArray(entryNum:Int, issueWidth:Int, wakeupWidth:Int, loadUnitNum:Int) extends XSModule{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))

    val selectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))
    val allocateInfo = Output(UInt(entryNum.W))

    val enq = Input(Valid(new Bundle{
      val addrOH = UInt(entryNum.W)
      val data = new IntegerStatusArrayEntry
    }))

    val issue = Input(Vec(issueWidth, Valid(UInt(entryNum.W))))
    val wakeup = Input(Vec(wakeupWidth, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
  })

  private val statusArray = Reg(Vec(entryNum, new IntegerStatusArrayEntry))
  private val statusArrayValid = RegInit(VecInit(Seq.fill(entryNum)(false.B)))
  private val statusArrayValidAux = RegInit(VecInit(Seq.fill(entryNum)(false.B)))

  //Start of select logic
  for(((selInfo, saEntry), saValid) <- io.selectInfo
    .zip(statusArray)
    .zip(statusArrayValid)){
    val entryToSelectInfoCvt = Module(new IntegerIssueInfoGenerator)
    entryToSelectInfoCvt.io.in.valid := saValid
    entryToSelectInfoCvt.io.in.bits := saEntry
    entryToSelectInfoCvt.io.earlyWakeUpCancel := io.earlyWakeUpCancel
    entryToSelectInfoCvt.io.redirect := io.redirect
    selInfo := entryToSelectInfoCvt.io.out
  }
  //End of select logic

  //Start of allocate logic
  io.allocateInfo := Cat(statusArrayValidAux.reverse)
  //End of allocate logic

  for((((v, va), d), idx) <- statusArrayValid
    .zip(statusArrayValidAux)
    .zip(statusArray)
    .zipWithIndex
  ){
    val updateNetwork = Module(new IntegerStatusArrayEntryUpdateNetwork(issueWidth, wakeupWidth))
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
      v  := updateNetwork.io.entryNext.valid
      va := updateNetwork.io.entryNext.valid
      d  := updateNetwork.io.entryNext.bits
    }
  }

  xs_assert(Cat(statusArrayValid) === Cat(statusArrayValidAux))
  xs_assert(Mux(io.enq.valid, PopCount(io.enq.bits.addrOH) === 1.U, true.B))
  xs_assert((Mux(io.enq.valid, io.enq.bits.addrOH, 0.U) & Cat(statusArrayValid.reverse)) === 0.U)
  for(iss <- io.issue){
    xs_assert(PopCount(Mux(iss.valid, iss.bits, 0.U) & Cat(statusArrayValid.reverse)) === 1.U)
  }
}
