package regfile
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import common.{ExuInput, Redirect, SrcType, XSParam}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.config.Parameters
import execute.fu.fpu.FMAMidResult
import writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}
import xs.utils.Assertion.xs_assert
class FloatingRegFile (val entriesNum:Int, name:String)(implicit p: Parameters) extends LazyModule with XSParam{
  private val wbNodeParam = WriteBackSinkParam(name, WriteBackSinkType.fpRf)
  val issueNode = new RegfileIssueNode
  val writeBackNode = new WriteBackSinkNode(wbNodeParam)
  lazy val module = new FloatingRegFileImpl(this)
}

class FloatingRegFileImpl(outer: FloatingRegFile)(implicit p: Parameters) extends LazyModuleImp(outer) with XSParam{
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
  }) ++ issueOut.map(_._1.fmaMidState.out.valid)
  private val fmaMidResultWidth = (new FMAMidResult).getWidth
  private val writebackTuples = writeBacks.map(elm => (elm._1.bits.uop.pdest, elm._1.bits.data))
  //Part of FMA middle state is stored in the pdest reg.
  private val feedbackTuples = issueOut
    .map(_._1.fmaMidState.out.bits)
    .map(elm => (elm.pdest, elm.midResult.asTypeOf(UInt(fmaMidResultWidth.W))(XLEN - 1, 0)))
  //Write Operations
  (writebackTuples ++ feedbackTuples).zip(wbEnables).foreach({
    case((dst, data), en) =>
      when(en){
        mem(dst) := data
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
      val lpvCancel = bi.issue.bits.uop.lpv.zip(io.earlyWakeUpCancel).map({ case (l, c) => l(1) & c }).reduce(_ || _)
      val outBundle = Wire(Valid(new ExuInput))
      outBundle.valid := bi.issue.valid && !lpvCancel && !bi.issue.bits.uop.robIdx.needFlush(io.redirect)
      outBundle.bits := bi.issue.bits
      outBundle.bits.src.take(eo.srcNum)
        .zip(bi.issue.bits.uop.psrc.take(eo.srcNum))
        .zip(bi.issue.bits.uop.ctrl.srcType.take(eo.srcNum))
        .zipWithIndex
        .foreach({ case (((data, addr), st), srcIdx) =>
          val bypassData = Wire(UInt(XLEN.W))
          val bypassValid = Wire(Bool())
          if(wbsWithBypass.nonEmpty) {
            val bypassOH = wbsWithBypass.map(_._1).map({ elm => elm.valid && elm.bits.uop.pdest === addr && !elm.bits.uop.robIdx.needFlush(io.redirect) })
            bypassData := Mux1H(bypassOH, wbsWithBypass.map(_._1.bits.data))
            bypassValid := Cat(bypassOH).orR
            xs_assert(PopCount(bypassOH) === 1.U)
          } else {
            bypassData := DontCare
            bypassValid := false.B
          }
          when(st === SrcType.fp) {
            if(srcIdx == 0) {
              val realAddr = Mux(bi.fmaMidState.in.valid, bi.issue.bits.uop.pdest, addr)
              val dataRead = mem.read(realAddr)
              data := Mux(bi.fmaMidState.in.valid, dataRead,
                Mux(bypassValid, bypassData, dataRead
                )
              )
              xs_assert(!bi.fmaMidState.in.valid || !bypassValid)
            } else {
              data := Mux(bypassValid, bypassData, mem.read(addr))
            }
          }
        })

      if (eo.srcNum < outBundle.bits.src.length) outBundle.bits.src.slice(eo.srcNum, outBundle.bits.src.length).foreach(_ := DontCare)


      val midStateAsUInt = Wire(UInt(fmaMidResultWidth.W))
      midStateAsUInt := Cat(bi.fmaMidState.in.bits.asUInt(fmaMidResultWidth - 1, XLEN), outBundle.bits.src(0))

      val issueValidReg = RegInit(false.B)
      val issueDataReg = Reg(new ExuInput)
      val midResultValidReg = RegInit(false.B)
      val midResultDataReg = Reg(new FMAMidResult)
      val waitForAddReg = RegInit(false.B)
      val permitFlow = !issueValidReg || bo.issue.fire
      bi.issue.ready := permitFlow
      when(permitFlow){
        issueValidReg := outBundle.valid
      }
      when(permitFlow && outBundle.valid){
        issueDataReg := outBundle.bits
        midResultValidReg := bi.fmaMidState.in.valid
        midResultDataReg := midStateAsUInt.asTypeOf(bo.fmaMidState.in.bits)
        waitForAddReg := bi.fmaMidState.waitForAdd
      }

      bo.issue.valid := issueValidReg
      bo.issue.bits := issueDataReg
      bo.fmaMidState.waitForAdd := waitForAddReg
      bo.fmaMidState.in.valid := midResultValidReg
      bo.fmaMidState.in.bits := midResultDataReg

      bi.fmaMidState.out := bo.fmaMidState.out
      bi.fuInFire := bo.issue.fire
    }
  }
}
