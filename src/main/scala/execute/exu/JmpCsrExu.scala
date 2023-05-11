package execute.exu
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import common.{ExuInput, ExuOutput, FuType, Redirect, XSBundle, XSParam}
import execute.fu.{FUWithRedirect, FuConfigs, FuOutput, FunctionUnit}
import execute.fu.jmp._
import execute.fu.fence._
import execute.fu.fpu.IntToFP
import issue.IssueBundle
import xs.utils.Assertion.xs_assert
import xs.utils.ParallelMux

class FenceIO(implicit p: Parameters) extends XSBundle {
  val sfence = Output(new SfenceBundle)
  val fencei = Output(Bool())
  val sbuffer = new FenceToSbuffer
}

class JmpCsrExu (id:Int, complexName:String, val bypassInNum:Int)(implicit p:Parameters) extends BasicExu{
  private val cfg = ExuConfig(
    name = "JmpCsrExu",
    id = id,
    complexName = complexName,
    fuConfigs = Seq(FuConfigs.jmpCfg, FuConfigs.fenceCfg, FuConfigs.mouCfg, FuConfigs.csrCfg),
    exuType = ExuType.jmp
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutputNode(cfg)
  override lazy val module = new JmpCsrExuImpl(this, cfg)
}
class FakeMou()(implicit p:Parameters) extends FunctionUnit(64){
  val issueToMou = IO(Decoupled(new ExuInput))
  val writebackFromMou = IO(Flipped(Decoupled(new ExuOutput)))
  io.in.ready := issueToMou.ready
  issueToMou.valid := io.in.valid
  issueToMou.bits.src := io.in.bits.src
  issueToMou.bits.uop := io.in.bits.uop
  io.out.valid := writebackFromMou.valid
  io.out.bits := writebackFromMou.bits
  writebackFromMou.ready := io.out.ready
}
class FakeCsr(implicit p:Parameters) extends FUWithRedirect(64){
  io.in.ready := true.B
  io.out := DontCare
  redirectOutValid := false.B
  redirectOut := DontCare
}

class JmpCsrExuImpl(outer:JmpCsrExu, exuCfg:ExuConfig)(implicit p:Parameters) extends BasicExuImpl(outer) with XSParam {
  val io = IO(new Bundle{
    val bypassIn = Input(Vec(outer.bypassInNum, Valid(new ExuOutput)))
    val fenceio = new FenceIO
    val issueToMou = Decoupled(new ExuInput)
    val writebackFromMou = Flipped(Decoupled(new ExuOutput))
  })
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1
  private val fence = Module(new Fence)
  private val jmp = Module(new Jump)
  private val mou = Module(new FakeMou)
  private val csr = Module(new FakeCsr)

  issuePort.issue.ready := true.B
  issuePort.fmaMidState.out := DontCare
  issuePort.fuInFire := DontCare

  private val finalIssueSignals = bypassSigGen(io.bypassIn, issuePort, outer.bypassInNum > 0)
  private val fuSeq = Seq(jmp, fence, mou, csr)
  fuSeq.zip(exuCfg.fuConfigs).foreach({ case (m, cfg) =>
    m.io.redirectIn := redirectIn
    m.io.in.valid := finalIssueSignals.valid && finalIssueSignals.bits.uop.ctrl.fuType === cfg.fuType
    m.io.in.bits.uop := finalIssueSignals.bits.uop
    m.io.in.bits.src := finalIssueSignals.bits.src
    m.io.out.ready := true.B

    val isJmp = finalIssueSignals.bits.uop.ctrl.fuType === FuType.jmp
    val isExclusive = finalIssueSignals.bits.uop.ctrl.noSpecExec && finalIssueSignals.bits.uop.ctrl.blockBackward
    xs_assert(Mux(m.io.in.valid, m.io.in.ready, true.B))
    xs_assert(isJmp || isExclusive)
  })

  private val fuOut = fuSeq.map(_.io.out)
  private val outSel = fuOut.map(_.fire)
  private val outData = fuOut.map(_.bits)
  private val finalData = ParallelMux(outSel, outData)

  writebackPort := DontCare
  writebackPort.valid := outSel.reduce(_ || _)
  writebackPort.bits.uop := finalData.uop
  writebackPort.bits.data := finalData.data

  xs_assert(PopCount(outSel) === 1.U || PopCount(outSel) === 0.U)

  io.fenceio.sfence := fence.sfence
  io.fenceio.fencei := fence.fencei
  io.fenceio.sbuffer <> fence.toSbuffer

  io.issueToMou <> mou.issueToMou
  io.writebackFromMou <> mou.writebackFromMou

  writebackPort.bits.fflags := DontCare
  writebackPort.bits.redirect := Mux(csr.redirectOutValid, csr.redirectOut, jmp.redirectOut)
  writebackPort.bits.redirectValid := jmp.redirectOutValid | csr.redirectOutValid


  //TODO: this signals should connect to csr
  fence.disableSfence := DontCare
}
