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

class IntegerIssueInfoGenerator extends Module{
  val io = IO(new Bundle{
    val in = Input(Valid(new IntegerStatusArrayEntry))
    val out = Output(Valid(new SelectInfo))
  })
  private val iv = io.in.valid
  private val ib = io.in.bits
  private val readyToIssue = ib.srcState(0) === SrcState.rdy & ib.srcState(1) === SrcState.rdy & !ib.issued
  io.out.valid := readyToIssue & iv
  io.out.bits.fuType := ib.fuType
  io.out.bits.robPtr := ib.robIdx
  io.out.bits.pdest := ib.pdest
  io.out.bits.fpWen := false.B
  io.out.bits.rfWen := ib.rfWen
  io.out.bits.lpv.zip(ib.lpv.transpose).foreach({case(o, i) => o := i.reduce(_|_)})
}
class IntegerStatusArrayEntry extends BasicStatusArrayEntry(2, true){
  val issued = Bool()
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
  private val miscUpdateEn = WireInit(false.B)
  private val enqNext = Wire(Valid(new IntegerStatusArrayEntry))
  private val enqUpdateEn = WireInit(false.B)
  private val redirectNext = WireInit(io.entry)

  //Start of wake up
  private val pregMatch = io.entry.bits.psrc
    .zip(io.entry.bits.srcType)
    .map(p => (io.wakeup ++ io.loadEarlyWakeup).map(elm => (elm.bits.pdest === p._1) && elm.valid && p._2 === SrcType.reg))
  for((n, v) <- miscNext.bits.srcState zip pregMatch){
    val shouldUpdateSrcState = Cat(v).orR
    when(shouldUpdateSrcState){
      n := SrcState.rdy
    }
  }
  //LPV update
  for((((newLpv, regIdx), regType), oldLpv) <- miscNext.bits.lpv
    .zip(io.entry.bits.psrc)
    .zip(io.entry.bits.srcType)
    .zip(io.entry.bits.lpv)){
    val shouldUpdateLpv = io.loadEarlyWakeup.map(elm => elm.valid && elm.bits.pdest === regIdx && SrcType.reg === regType)
    for((((nl, v), d), ol) <- newLpv.zip(shouldUpdateLpv).zip(io.loadEarlyWakeup).zip(oldLpv)){
      nl := Mux(v, d.bits.lpv, Cat(0.U(1.W), ol(ol.getWidth-1,1)))
    }
  }
  private val miscUpdateEn0 = pregMatch.map(_.reduce(_|_)).reduce(_|_)
  //End of wake up

  //Start of issue and cancel
  private val shouldBeCancelled = io.earlyWakeUpCancel.zip(io.entry.bits.lpv.transpose).map({ case(c, l) => l.map(_(0) & c).reduce(_|_)}).reduce(_|_)
  private val shouldBeIssued = Cat(io.issue).orR
  miscNext.bits.issued := Mux(shouldBeCancelled, false.B, Mux(shouldBeIssued, true.B, io.entry.bits.issued))
  private val miscUpdateEn1 = Cat(shouldBeCancelled, shouldBeIssued).orR
  //End of issue and cancel

  //Start of dequeue
  private val mayNeedReplay = io.entry.bits.lpv.map(_.map(_.orR).reduce(_|_)).reduce(_|_)
  when(io.entry.bits.issued & !mayNeedReplay){
    miscNext.valid := false.B
    miscUpdateEn := true.B
  }
  //End of dequeue

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
  private val shouldBeFlushed = io.entry.valid & Mux(!io.redirect.valid, false.B,
    Mux(flushItself(io.redirect.bits.level), io.entry.bits.robIdx > io.redirect.bits.robIdx,
      io.entry.bits.robIdx >= io.redirect.bits.robIdx
    ))
  redirectNext.valid := false.B
  //End of redirect

  io.updateEnable := shouldBeFlushed | enqUpdateEn | mayNeedReplay | miscUpdateEn1 | miscUpdateEn0
  io.entryNext := Mux(enqUpdateEn, enqNext, Mux(shouldBeFlushed, redirectNext, miscNext))
}

class IntegerStatusArray(entryNum:Int, issueWidth:Int, wakeupWidth:Int, loadUnitNum:Int) extends XSModule{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))

    val issueInfo = Output(Vec(entryNum, Valid(new SelectInfo)))
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
  for(((issInfo, saEntry), saValid) <- io.issueInfo
    .zip(statusArray)
    .zip(statusArrayValid)){
    val entryToSelectInfoCvt = Module(new IntegerIssueInfoGenerator)
    entryToSelectInfoCvt.io.in.valid := saValid
    entryToSelectInfoCvt.io.in.bits := saEntry
    issInfo := entryToSelectInfoCvt.io.out
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
