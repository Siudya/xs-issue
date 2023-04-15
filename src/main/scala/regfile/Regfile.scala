package regfile
import chisel3._
import chisel3.util._
import common.{ExuInput, FuType, MicroOp, Redirect, XSParam}
import exu.ExuType
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.config.Parameters
import issue.IssueBundle
import writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}
import xs.utils.Assertion.xs_assert
class Regfile(val entriesNum:Int, val typeString:String = "Int", name:String)(implicit p: Parameters) extends LazyModule with XSParam{
  require(typeString == "Int" || typeString == "Fp")
  val isInt = typeString == "Int"
  private val wbNodeParam = WriteBackSinkParam(name, if(isInt) WriteBackSinkType.intRf else WriteBackSinkType.fpRf)
  val issueNode = new RegfileIssueNode
  val writeBackNode = new WriteBackSinkNode(wbNodeParam)
  lazy val module = new RegfileImpl(this)
}

class RegfileImpl(outer: Regfile)(implicit p: Parameters) extends LazyModuleImp(outer) with XSParam{
  private val isInt = outer.isInt
  private val issueIn = outer.issueNode.in.head._1 zip outer.issueNode.in.head._2
  private val issueOut = outer.issueNode.out
  private val jmpNum = issueOut.map(_._2).count(_.exuType == ExuType.jmp)
  require(jmpNum <= 1, "Only one jmp module is supported!")
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val jmpTargetRead = Output(UInt(if(jmpNum > 0) log2Ceil(FtqSize).W else 0.W))
    val jmpTargetData = Input(UInt(if(jmpNum > 0) VAddrBits.W else 0.W))
    val jmpPcRead = Output(UInt(if(jmpNum > 0) log2Ceil(FtqSize).W else 0.W))
    val jmpPcData = Input(UInt(if(jmpNum > 0) VAddrBits.W else 0.W))
  })
  private val wbIn = outer.writeBackNode.in.head
  private val writeBacks = wbIn._1.zip(wbIn._2)
  private val mem = Mem(outer.entriesNum, UInt(XLEN.W))
  private val wbEnables = writeBacks.map({case(b, _) =>
    val correctRf = if(isInt) b.bits.uop.ctrl.rfWen else b.bits.uop.ctrl.fpWen
    correctRf && b.valid && !b.bits.uop.robIdx.needFlush(io.redirect)
  })
  writeBacks.zip(wbEnables).foreach({
    case((b, _), en) =>
      when(en){
        mem(b.bits.uop.pdest) := b.bits.data
      }
  })
  private val wbsWithBypass = writeBacks.filter(_._2.hasFastWakeup)
  private val issueOutMap = issueOut.map(elm => elm._2 -> (elm._1, elm._2)).toMap
  for(issI <- issueIn) {
    val issO = issueOutMap(issI._2)
    val bi = issI._1
    val bo = issO._1
    val eo = issO._2
    val outBundle = Wire(new IssueBundle)
    outBundle := DontCare
    outBundle.issue.ready := DontCare
    outBundle.issue.valid := bi.issue.valid
    outBundle.issue.bits := bi.issue.bits
    outBundle.issue.valid := bi.issue.valid && !bi.issue.bits.uop.robIdx.needFlush(io.redirect)
    outBundle.issue.bits.src.take(eo.srcNum).zip(bi.issue.bits.uop.psrc.take(eo.srcNum)).foreach({ case (data, addr) =>
      val bypassOH = wbsWithBypass.map(_._1.bits.uop.pdest).zip(wbEnables).map({ case (dst, en) => en & dst === addr })
      val bypassData = Mux1H(bypassOH, writeBacks.map(_._1.bits.data))
      val bypassValid = Cat(bypassOH).orR
      if (isInt) {
        data := Mux(addr === 0.U, 0.U, Mux(bypassValid, bypassData, mem(addr)))
      } else {
        data := Mux(bypassValid, bypassData, mem(addr))
      }
      xs_assert(PopCount(bypassOH) === 1.U)
    })

    if (eo.srcNum < outBundle.issue.bits.src.length) outBundle.issue.bits.src.slice(eo.srcNum, outBundle.issue.bits.src.length).foreach(_ := DontCare)
    if (eo.exuType == ExuType.jmp) {
      io.jmpTargetRead := (bi.issue.bits.uop.cf.ftqPtr + 1.U).value
      io.jmpPcRead := bi.issue.bits.uop.cf.ftqPtr.value
      outBundle.issue.bits.uop.cf.pc := io.jmpPcData
      when(bi.issue.bits.uop.ctrl.fuType === FuType.jmp) {
        outBundle.issue.bits.src(1) := io.jmpTargetData
      }
    }
    val outputValidDriverRegs = RegInit(false.B)
    val outputExuInputDriverRegs = Reg(new ExuInput)
    val outputfmaWaitForAddDriverRegs = RegInit(false.B)
    val pipelinePermitted = (!outputValidDriverRegs) || bo.issue.fire
    when(pipelinePermitted) {
      outputValidDriverRegs := outBundle.issue.valid
    }
    when(pipelinePermitted && outBundle.issue.valid) {
      outputExuInputDriverRegs := outBundle.issue.bits
    }

    bo.issue.valid := outputValidDriverRegs
    bo.issue.bits := outputExuInputDriverRegs
    bo.fmaWaitForAdd := outputfmaWaitForAddDriverRegs
    bo.fmaMidStateIssue.valid := RegNext(bi.fmaMidStateIssue.valid, false.B)
    bo.fmaMidStateIssue.bits := RegEnable(bi.fmaMidStateIssue.bits, bi.fmaMidStateIssue.valid)
    bi.issue.ready := pipelinePermitted
    bi.fmaMidStateFeedBack := bo.fmaMidStateFeedBack
  }
}