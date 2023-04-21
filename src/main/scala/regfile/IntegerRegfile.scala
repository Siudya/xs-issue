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
import xs.utils.LogicShiftRight
class IntegerRegFile(val entriesNum:Int, name:String)(implicit p: Parameters) extends LazyModule with XSParam{
  private val wbNodeParam = WriteBackSinkParam(name, WriteBackSinkType.intRf)
  val issueNode = new RegfileIssueNode
  val writeBackNode = new WriteBackSinkNode(wbNodeParam)
  lazy val module = new IntegerRegFileImpl(this)
}

class IntegerRegFileImpl(outer: IntegerRegFile)(implicit p: Parameters) extends LazyModuleImp(outer) with XSParam{
  private val issueIn = outer.issueNode.in.head._1 zip outer.issueNode.in.head._2
  private val issueOut = outer.issueNode.out
  private val jmpNum = issueOut.map(_._2).count(_.exuType == ExuType.jmp)
  require(jmpNum <= 1, "Only one jmp module is supported!")
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val jmpTargetRead = Output(UInt(if(jmpNum > 0) log2Ceil(FtqSize).W else 0.W))
    val jmpTargetData = Input(UInt(if(jmpNum > 0) VAddrBits.W else 0.W))
    val jmpPcRead = Output(UInt(if(jmpNum > 0) log2Ceil(FtqSize).W else 0.W))
    val jmpPcData = Input(UInt(if(jmpNum > 0) VAddrBits.W else 0.W))
  })
  private val wbIn = outer.writeBackNode.in.head
  private val writeBacks = wbIn._1.zip(wbIn._2)
  private val mem = Mem(outer.entriesNum, UInt(XLEN.W))
  private val wbEnables = writeBacks.map({case(b, _) =>
    b.bits.uop.ctrl.rfWen && b.valid && !b.bits.uop.robIdx.needFlush(io.redirect)
  })
  writeBacks.zip(wbEnables).foreach({
    case((b, _), en) =>
      when(en){
        mem(b.bits.uop.pdest) := b.bits.data
      }
  })
  private val wbsWithBypass = writeBacks.filter(elm => elm._2.hasFastWakeup || elm._2.isMemType)
  private val issueOutMap = issueOut.map(elm => elm._2 -> (elm._1, elm._2)).toMap
  for(issI <- issueIn) {
    val issO = issueOutMap(issI._2)
    val bi = issI._1
    val bo = issO._1
    val eo = issO._2
    prefix(eo.name + "_" + eo.id) {
      val outBundle = Wire(Valid(new ExuInput))
      val lpvCancel = bi.issue.bits.uop.lpv.zip(io.earlyWakeUpCancel).map({ case (l, c) => l(1) & c }).reduce(_ || _)
      outBundle.valid := bi.issue.valid && !lpvCancel && !bi.issue.bits.uop.robIdx.needFlush(io.redirect)
      outBundle.bits := bi.issue.bits
      outBundle.bits.src.take(eo.srcNum)
        .zip(bi.issue.bits.uop.psrc.take(eo.srcNum))
        .zip(bi.issue.bits.uop.ctrl.lsrc.take(eo.srcNum))
        .zip(bi.issue.bits.uop.ctrl.srcType.take(eo.srcNum))
        .foreach({ case (((data, paddr), laddr), st) =>
          val bypassOH = wbsWithBypass.map(_._1.bits.uop.pdest).zip(wbEnables).map({ case (dst, en) => en & dst === paddr })
          val bypassData = Mux1H(bypassOH, wbsWithBypass.map(_._1.bits.data))
          val bypassValid = Cat(bypassOH).orR
          when(st === SrcType.reg) {
            data := Mux(laddr === 0.U, 0.U, Mux(bypassValid, bypassData, mem(paddr)))
          }
          xs_assert(PopCount(bypassOH) === 1.U)
        })

      if (eo.srcNum < outBundle.bits.src.length) outBundle.bits.src.slice(eo.srcNum, outBundle.bits.src.length).foreach(_ := DontCare)
      val imJmp = eo.exuType == ExuType.jmp
      if (imJmp) {
        io.jmpTargetRead := (bi.issue.bits.uop.cf.ftqPtr + 1.U).value
        io.jmpPcRead := bi.issue.bits.uop.cf.ftqPtr.value
      }
      val realIssueOut = Wire(new ExuInput)
      realIssueOut := ImmExtractor(eo, outBundle.bits, if (imJmp) Some(io.jmpPcData) else None, if (imJmp) Some(io.jmpTargetData) else None)

      val pipeline = Module(new DecoupledPipeline(eo.latency == Int.MaxValue))
      pipeline.io.redirect := io.redirect

      pipeline.io.enq.issue.valid := outBundle.valid
      pipeline.io.enq.issue.bits := realIssueOut
      bi.issue.ready := pipeline.io.enq.issue.ready
      pipeline.io.enq.fmaMidState.waitForAdd := bi.fmaMidState.waitForAdd
      pipeline.io.enq.fmaMidState.in := bi.fmaMidState.in

      bo.issue.valid := pipeline.io.deq.issue.valid
      bo.issue.bits := pipeline.io.deq.issue.bits
      pipeline.io.deq.issue.ready := bo.issue.ready
      pipeline.io.deq.fmaMidState.out := DontCare
      bo.fmaMidState.waitForAdd := pipeline.io.deq.fmaMidState.waitForAdd
      bo.fmaMidState.in := pipeline.io.deq.fmaMidState.in

      bi.fmaMidState.out := DontCare
    }
  }
}