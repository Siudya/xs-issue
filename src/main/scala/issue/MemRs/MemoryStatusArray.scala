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
package issue.MemRs
import chisel3._
import chisel3.experimental.ChiselAnnotation
import chisel3.util._
import common.{FuOpType, FuType, Redirect, RobPtr, SqPtr, SrcState, SrcType, XSModule}
import firrtl.passes.InlineAnnotation
import issue._
import xs.utils.Assertion.xs_assert
import xs.utils.LogicShiftRight
import issue.MemRs.EntryState._

protected[MemRs] object EntryState{
  def s_wait_st:UInt = 0.U
  def s_ready:UInt = 1.U
  def s_wait_cancel: UInt = 2.U
  def s_wait_replay: UInt = 3.U
  def s_wait_counter:UInt = 4.U
  def s_issued: UInt = 5.U
  def apply() = UInt(3.W)
}

sealed class BasicMemoryIssueInfoGenerator extends XSModule{
  val io = IO(new Bundle{
    val in = Input(Valid(new MemoryStatusArrayEntry))
    val out = Output(Valid(new SelectInfo))
    val redirect = Input(Valid(new Redirect))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
  })
  private val iv = io.in.valid
  protected val ib = io.in.bits
  private val shouldBeFlushed = ib.robIdx.needFlush(io.redirect)
  private val shouldBeCanceled = ib.lpv.map(l => l.zip(io.earlyWakeUpCancel).map({ case (li, c) => li(1) && c }).reduce(_ || _)).reduce(_ || _)
  protected val readyToIssue = Wire(Bool())
  io.out.valid := readyToIssue && iv && !shouldBeFlushed && !shouldBeCanceled
  io.out.bits.fuType := ib.fuType
  io.out.bits.robPtr := ib.robIdx
  io.out.bits.pdest := ib.pdest
  io.out.bits.fpWen := ib.fpWen
  io.out.bits.rfWen := ib.rfWen
  io.out.bits.fmaWaitAdd := false.B
  io.out.bits.midResultReadEn := false.B
  io.out.bits.lpv.zip(ib.lpv.transpose).foreach({case(o, i) => o := i.reduce(_|_)})
  chisel3.experimental.annotate(new ChiselAnnotation {
    def toFirrtl = InlineAnnotation(toNamed)
  })
}

class StaIssueInfoGen extends BasicMemoryIssueInfoGenerator{
  readyToIssue := ib.srcState(0) === SrcState.rdy && ib.staLoadState === EntryState.s_ready
  io.out.bits.fpWen := false.B
  io.out.bits.rfWen := false.B
}
class LoadIssueInfoGen extends BasicMemoryIssueInfoGenerator{
  readyToIssue := ib.srcState(0) === SrcState.rdy && ib.staLoadState === EntryState.s_ready
}

class StdIssueInfoGen extends BasicMemoryIssueInfoGenerator{
  readyToIssue := ib.srcState(1) === SrcState.rdy && ib.stdMouState === EntryState.s_ready
}

class MouIssueInfoGen extends BasicMemoryIssueInfoGenerator{
  readyToIssue := ib.srcState(0) === SrcState.rdy && ib.srcState(1) === SrcState.rdy
}

class MemoryStatusArrayEntry extends BasicStatusArrayEntry(2, false){
  val staLoadState = EntryState()
  val stdMouState = EntryState()
  val counter = UInt(7.W)
  val isFirstIssue = Bool()
  val waitTarget = new RobPtr
}

class MemoryStatusArrayEntryUpdateNetwork(stuNum:Int, lduNum:Int, mouNum:Int, wakeupWidth:Int) extends Module{
  val io = IO(new Bundle{
    val entry = Input(Valid(new MemoryStatusArrayEntry))
    val entryNext = Output(Valid(new MemoryStatusArrayEntry))
    val updateEnable = Output(Bool())
    val enq = Input(Valid(new MemoryStatusArrayEntry))
    val staIssue = Input(Vec(stuNum, Bool()))
    val stdIssue = Input(Vec(stuNum, Bool()))
    val lduIssue = Input(Vec(lduNum, Bool()))
    val mouIssue = Input(Vec(mouNum, Bool()))
    val wakeup = Input(Vec(wakeupWidth, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Input(Vec(lduNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(lduNum, Bool()))
    val stIssued = Input(Vec(stuNum, Valid(new RobPtr)))
    val replay = Input(Valid(UInt(5.W)))
    val redirect = Input(Valid(new Redirect))
  })

  io.entryNext := io.entry
  private val miscNext = WireInit(io.entry)
  private val enqNext = Wire(Valid(new MemoryStatusArrayEntry))
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
  private val imLoad = io.entry.bits.fuType === FuType.ldu
  private val imStore = io.entry.bits.fuType === FuType.stu
  private val imAtomic = io.entry.bits.fuType === FuType.mou
  private val needReplay = io.replay.valid
  private val counter = io.entry.bits.counter
  private val counterNext = miscNext.bits.counter
  private val srcShouldBeCancelled = io.entry.bits.lpv.map(l => io.earlyWakeUpCancel.zip(l).map({ case(c, li) => li(0) & c}).reduce(_|_))
  private val src0HasSpecWakeup = io.entry.bits.lpv(0).map(_.orR).reduce(_||_)
  private val src1HasSpecWakeup = io.entry.bits.lpv(1).map(_.orR).reduce(_||_)
  private val stIssueHit = io.stIssued.map(elm => elm.valid && elm.bits === io.entry.bits.waitTarget).reduce(_|_)
  private val staLoadIssued = Cat(io.staIssue ++ io.lduIssue).orR
  private val stdMouIssued = Cat(io.stdIssue ++ io.mouIssue).orR
  private val staLoadState = io.entry.bits.staLoadState
  private val stdMouState = io.entry.bits.stdMouState
  private val staLoadStateNext = io.entryNext.bits.staLoadState
  private val stdMouStateNext = io.entryNext.bits.stdMouState
  private val miscUpdateEnCancelOrIssue = WireInit(false.B)

  miscUpdateEnCancelOrIssue := needReplay || counter.orR || src0HasSpecWakeup || src1HasSpecWakeup || stIssueHit || staLoadIssued || stdMouIssued

  when(imLoad) {
    switch(staLoadState) {
      is(s_wait_st) {
        when(stIssueHit) {
          staLoadStateNext := s_ready
        }
      }
      is(s_ready) {
        when(staLoadIssued) {
          staLoadStateNext := Mux(src0HasSpecWakeup, s_wait_cancel, s_wait_replay)
        }
      }
      is(s_wait_cancel){
        when(srcShouldBeCancelled(0)){
          staLoadStateNext := s_ready
        }.elsewhen(!src0HasSpecWakeup){
          staLoadStateNext := s_wait_replay
        }
      }
      is(s_wait_replay){
        when(needReplay){
          staLoadStateNext := s_wait_counter
        }.elsewhen(counter === 1.U){
          staLoadStateNext := s_issued
        }
      }
      is(s_wait_counter){
        when(counter === 1.U){
          staLoadStateNext := s_ready
        }
      }
    }
  }

  when(imStore) {
    //STA State machine
    switch(staLoadState){
      is(s_ready){
        when(staLoadIssued) {
          staLoadStateNext := Mux(src0HasSpecWakeup, s_wait_cancel, s_issued)
        }
      }
      is(s_wait_cancel){
        when(srcShouldBeCancelled(0)){
          staLoadStateNext := s_ready
        }.elsewhen(!src0HasSpecWakeup){
          staLoadStateNext := s_issued
        }
      }
    }
    //STD State machine
    switch(stdMouState) {
      is(s_ready) {
        when(stdMouIssued) {
          stdMouStateNext := Mux(src1HasSpecWakeup, s_wait_cancel, s_issued)
        }
      }
      is(s_wait_cancel) {
        when(srcShouldBeCancelled(1)) {
          stdMouStateNext := s_ready
        }.elsewhen(!src1HasSpecWakeup) {
          stdMouStateNext := s_issued
        }
      }
    }
  }
  private val shouldBeCanceled = srcShouldBeCancelled.reduce(_|_)
  private val maybeCanceled = src0HasSpecWakeup || src1HasSpecWakeup
  when(imAtomic){
    switch(stdMouState){
      is(s_ready){
        when(stdMouIssued) {
          stdMouStateNext := Mux(maybeCanceled, s_wait_cancel, s_issued)
        }
      }
      is(s_wait_cancel) {
        when(shouldBeCanceled) {
          stdMouStateNext := s_ready
        }.elsewhen(!maybeCanceled) {
          stdMouStateNext := s_issued
        }
      }
    }
  }

  when(io.replay.valid){
    counterNext := io.replay.bits
  }.elsewhen(staLoadStateNext === s_wait_replay && staLoadState =/= s_wait_replay) {
    counterNext := (1 << 4).U
  }.elsewhen(counter.orR) {
    counterNext := LogicShiftRight(counter, 1)
  }

  xs_assert(Mux(imLoad, stdMouState === s_issued, true.B))
  xs_assert(Mux(imStore, staLoadState === s_ready || staLoadState === s_wait_cancel || staLoadState === s_issued, true.B))
  xs_assert(Mux(imStore || imAtomic, stdMouState === s_ready || stdMouState === s_wait_cancel || stdMouState === s_issued, true.B))
  xs_assert(Mux(imAtomic, staLoadState === s_issued, true.B))

  srcShouldBeCancelled.zip(miscNext.bits.srcState).foreach{case(en, state) => when(en){state := SrcState.busy}}
  //End of issue and cancel

  //Start of dequeue and redirect
  private val shouldBeFlushed = io.entry.valid & io.entry.bits.robIdx.needFlush(io.redirect)
  private val miscUpdateEnDequeueOrRedirect =
    (io.entry.bits.stdMouState === EntryState.s_issued && io.entry.bits.staLoadState === EntryState.s_issued) || shouldBeFlushed
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

class MemoryStatusArray(entryNum:Int, stuNum:Int, lduNum:Int, mouNum:Int, wakeupWidth:Int) extends XSModule{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))

    val staSelectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))
    val stdSelectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))
    val lduSelectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))
    val mouSelectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))

    val allocateInfo = Output(UInt(entryNum.W))

    val enq = Input(Valid(new Bundle{
      val addrOH = UInt(entryNum.W)
      val data = new MemoryStatusArrayEntry
    }))

    val staIssue = Input(Vec(stuNum, Valid(UInt(entryNum.W))))
    val stdIssue = Input(Vec(stuNum, Valid(UInt(entryNum.W))))
    val lduIssue = Input(Vec(lduNum, Valid(UInt(entryNum.W))))
    val mouIssue = Input(Vec(mouNum, Valid(UInt(entryNum.W))))
    val wakeup = Input(Vec(wakeupWidth, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val replay = Input(Vec(lduNum, Valid(new Bundle {
      val entryIdxOH = UInt(entryNum.W)
      val waitVal = UInt(5.W)
    })))
  })

  private val statusArray = Reg(Vec(entryNum, new MemoryStatusArrayEntry))
  private val statusArrayValid = RegInit(VecInit(Seq.fill(entryNum)(false.B)))
  private val statusArrayValidAux = RegInit(VecInit(Seq.fill(entryNum)(false.B)))

  //Start of select logic
  private val selInfoOut = Seq(io.staSelectInfo, io.stdSelectInfo, io.lduSelectInfo, io.mouSelectInfo)
  private val infoGenSeq = Seq(
    Seq.fill(entryNum)(Module(new StaIssueInfoGen)),
    Seq.fill(entryNum)(Module(new StdIssueInfoGen)),
    Seq.fill(entryNum)(Module(new LoadIssueInfoGen)),
    Seq.fill(entryNum)(Module(new MouIssueInfoGen))
  )
  for((so, gens) <- selInfoOut.zip(infoGenSeq)){
    for ((((selInfo, saEntry), saValid), cvt) <- so
      .zip(statusArray)
      .zip(statusArrayValid)
      .zip(gens)){
      cvt.io.in.valid := saValid
      cvt.io.in.bits := saEntry
      cvt.io.earlyWakeUpCancel := io.earlyWakeUpCancel
      cvt.io.redirect := io.redirect
      selInfo := cvt.io.out
    }
  }
  //End of select logic

  //Start of allocate logic
  io.allocateInfo := Cat(statusArrayValidAux.reverse)
  //End of allocate logic
  private val stIssuedValidRegs = Reg(VecInit(Seq.fill(stuNum)(false.B)))
  private val stIssuedDataRegs = Reg(Vec(stuNum, new RobPtr))
  stIssuedValidRegs.zip(stIssuedDataRegs).zip(io.staIssue).foreach({case((v, d), in) =>
    v := in.valid
    when(in.valid){
      d := in.bits
    }
  })
  for((((v, va), d), idx) <- statusArrayValid
    .zip(statusArrayValidAux)
    .zip(statusArray)
    .zipWithIndex
      ){
    val updateNetwork = Module(new MemoryStatusArrayEntryUpdateNetwork(stuNum, lduNum, mouNum, wakeupWidth))
    updateNetwork.io.entry.valid := v
    updateNetwork.io.entry.bits := d
    updateNetwork.io.enq.valid := io.enq.valid & io.enq.bits.addrOH(idx)
    updateNetwork.io.enq.bits := io.enq.bits.data
    updateNetwork.io.staIssue := VecInit(io.staIssue.map(i => i.valid & i.bits(idx)))
    updateNetwork.io.stdIssue := VecInit(io.stdIssue.map(i => i.valid & i.bits(idx)))
    updateNetwork.io.lduIssue := VecInit(io.lduIssue.map(i => i.valid & i.bits(idx)))
    updateNetwork.io.mouIssue := VecInit(io.mouIssue.map(i => i.valid & i.bits(idx)))
    updateNetwork.io.wakeup := io.wakeup
    updateNetwork.io.loadEarlyWakeup := io.loadEarlyWakeup
    updateNetwork.io.earlyWakeUpCancel := io.earlyWakeUpCancel

    updateNetwork.io.replay.valid := io.replay.valid & io.replay.bits.entryIdxOH(idx)
    updateNetwork.io.replay.bits := io.replay.bits.waitVal
    updateNetwork.io.redirect := io.redirect

    updateNetwork.io.stIssued.zip(stIssuedValidRegs).zip(stIssuedDataRegs).foreach({case((p, v), d) =>
      p.valid := v
      p.bits := d
    })

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
  private val issues = io.staIssue ++ io.stdIssue ++ io.lduIssue ++ io.mouIssue
  for(iss <- issues){
    xs_assert(PopCount(Mux(iss.valid, iss.bits, 0.U) & Cat(statusArrayValid.reverse)) === 1.U)
  }
}
