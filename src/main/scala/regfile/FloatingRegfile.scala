package regfile
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import common.{ExuInput, FuType, Redirect, SrcType, XSParam}
import exu.ExuType
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.config.Parameters
import issue.IssueBundle
import writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}
import xs.utils.Assertion.xs_assert
class FloatPointRegFile (val entriesNum:Int, name:String)(implicit p: Parameters) extends LazyModule with XSParam{
  private val wbNodeParam = WriteBackSinkParam(name, WriteBackSinkType.fpRf)
  val issueNode = new RegfileIssueNode
  val writeBackNode = new WriteBackSinkNode(wbNodeParam)
  lazy val module = new FloatPointRegFileImpl(this)
}

class FloatPointRegFileImpl(outer: FloatPointRegFile)(implicit p: Parameters) extends LazyModuleImp(outer) with XSParam{
  private val issueIn = outer.issueNode.in.head._1 zip outer.issueNode.in.head._2
  private val issueOut = outer.issueNode.out
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
  })
  private val wbIn = outer.writeBackNode.in.head
  private val writeBacks = wbIn._1.zip(wbIn._2)
  private val mem = Mem(outer.entriesNum, UInt(XLEN.W))
  private val wbEnables = writeBacks.map({case(b, _) =>
    b.bits.uop.ctrl.fpWen && b.valid && !b.bits.uop.robIdx.needFlush(io.redirect)
  })
  writeBacks.zip(wbEnables).foreach({
    case((b, _), en) =>
      when(en){
        mem(b.bits.uop.pdest) := b.bits.data
      }
  })
  private val wbsWithBypass = writeBacks.filter(_._2.isMemType)
  private val issueOutMap = issueOut.map(elm => elm._2 -> (elm._1, elm._2)).toMap
  for(issI <- issueIn) {
    val issO = issueOutMap(issI._2)
    val bi = issI._1
    val bo = issO._1
    val eo = issO._2
    prefix(eo.name + "_" + eo.id) {
      val lpvCancel = bi.issue.bits.uop.lpv.zip(io.earlyWakeUpCancel).map({ case (i, c) => i(0).asBool & c }).reduce(_ || _)
      val outBundle = Wire(Valid(new ExuInput))
      outBundle.valid := bi.issue.valid && !lpvCancel && !bi.issue.bits.uop.robIdx.needFlush(io.redirect)
      outBundle.bits := bi.issue.bits
      outBundle.bits.src.take(eo.srcNum)
        .zip(bi.issue.bits.uop.psrc.take(eo.srcNum))
        .zip(bi.issue.bits.uop.ctrl.srcType.take(eo.srcNum))
        .zipWithIndex
        .foreach({ case (((data, addr), st), idx) =>
          val bypassOH = wbsWithBypass.map(_._1.bits.uop.pdest).zip(wbEnables).map({ case (dst, en) => en & dst === addr })
          val bypassData = Mux1H(bypassOH, writeBacks.map(_._1.bits.data))
          val bypassValid = Cat(bypassOH).orR
          val realAddr = if (idx == 0) Mux(bi.fmaMidStateIssue.valid, bi.issue.bits.uop.pdest, addr) else addr
          when(st === SrcType.fp) {
            data := Mux(bypassValid, bypassData, mem(realAddr))
          }
          xs_assert(PopCount(bypassOH) === 1.U)
        })

      if (eo.srcNum < outBundle.bits.src.length) outBundle.bits.src.slice(eo.srcNum, outBundle.bits.src.length).foreach(_ := DontCare)
      val outputValidDriverRegs = RegInit(false.B)
      val outputExuInputDriverRegs = Reg(new ExuInput)
      val pipelinePermitted = (!outputValidDriverRegs) || bo.issue.fire
      when(pipelinePermitted) {
        outputValidDriverRegs := outBundle.valid
      }
      when(pipelinePermitted && outBundle.valid) {
        outputExuInputDriverRegs := outBundle.bits
      }

      bo.issue.valid := outputValidDriverRegs
      bo.issue.bits := outputExuInputDriverRegs
      bo.fmaWaitForAdd := RegNext(bi.fmaWaitForAdd, false.B)
      bo.fmaMidStateIssue.valid := RegNext(bi.fmaMidStateIssue.valid, false.B)
      val midStateAsUInt = Wire(UInt(bi.fmaMidStateIssue.bits.getWidth.W))
      midStateAsUInt(XLEN - 1, 0) := outBundle.bits.src(0)
      midStateAsUInt(bi.fmaMidStateIssue.bits.getWidth - 1, XLEN) := bi.fmaMidStateIssue.bits.asUInt(bi.fmaMidStateIssue.bits.getWidth - 1, XLEN)
      bo.fmaMidStateIssue.bits := RegEnable(midStateAsUInt.asTypeOf(bo.fmaMidStateIssue.bits), bi.fmaMidStateIssue.valid)
      bi.issue.ready := pipelinePermitted
      bi.fmaMidStateFeedBack := bo.fmaMidStateFeedBack
    }
  }
}
