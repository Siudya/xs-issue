package exu
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import common.{ExuOutput, FuType, Redirect, XSBundle, XSParam}
import fu.{FuConfigs, FuOutput}
import fu.jmp._
import fu.fence._
import fu.fpu.IntToFP
import xs.utils.Assertion.xs_assert

class FenceIO(implicit p: Parameters) extends XSBundle {
  val sfence = Output(new SfenceBundle)
  val fencei = Output(Bool())
  val sbuffer = new FenceToSbuffer
}

class JmpCsrExu (id:Int, val bypassInNum:Int)(implicit p:Parameters) extends BasicExu{
  private val cfg = ExuConfig(
    name = "JmpExu",
    id = id,
    blockName = "IntegerBlock",
    fuConfigs = Seq(FuConfigs.jmpCfg, FuConfigs.fenceCfg, FuConfigs.i2fCfg),
    exuType = ExuType.jmp
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutNode(cfg)
  override lazy val module = new JmpCsrExuImpl(this, cfg)
}
class JmpCsrExuImpl(outer:JmpCsrExu, exuCfg:ExuConfig)(implicit p:Parameters) extends BasicExuImpl(outer) with XSParam {
  val io = IO(new Bundle{
    val bypassIn = Input(Vec(outer.bypassInNum, Valid(new ExuOutput)))
    val fenceio = new FenceIO
  })
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1
  private val fence = Module(new Fence)
  private val jmp = Module(new Jump)
  private val i2f = Module(new IntToFP)
  private val outputArbiter = Module(new Arbiter(new FuOutput(XLEN), exuCfg.fuConfigs.length))

  private val finalIssueSignals = bypassSigGen(io.bypassIn, issuePort, outer.bypassInNum > 0)

  private val fuList = Seq(jmp, fence, i2f)
  private val fuReadies = exuCfg.fuConfigs.zip(fuList).zip(outputArbiter.io.in).map({case((cfg, fu), arbIn) =>
    val fuHit = finalIssueSignals.bits.uop.ctrl.fuType === cfg.fuType
    fu.io.redirectIn := redirectIn
    fu.io.in.valid := finalIssueSignals.valid & fuHit
    fu.io.in.bits.uop := finalIssueSignals.bits.uop
    fu.io.in.bits.src := finalIssueSignals.bits.src
    arbIn <> fu.io.out
    fuHit && fu.io.in.ready
  })
  private val inFuHits = exuCfg.fuConfigs.map({cfg => finalIssueSignals.bits.uop.ctrl.fuType === cfg.fuType})
  xs_assert(Mux(issuePort.issue.valid, PopCount(Cat(inFuHits)) === 1.U, true.B))
  issuePort.issue.ready := Mux1H(inFuHits, fuReadies)
  issuePort.fmaMidState.out := DontCare

  writebackPort.valid := outputArbiter.io.out.valid
  writebackPort.bits.uop := outputArbiter.io.out.bits.uop
  writebackPort.bits.data := outputArbiter.io.out.bits.data
  outputArbiter.io.out.ready := true.B

  io.fenceio.sfence := fence.sfence
  io.fenceio.fencei := fence.fencei
  io.fenceio.sbuffer <> fence.toSbuffer

  i2f.rm := finalIssueSignals.bits.uop.ctrl.fpu.rm
  writebackPort.bits.fflags := i2f.fflags
  writebackPort.bits.redirect := jmp.redirectOut
  writebackPort.bits.redirectValid := jmp.redirectOutValid

  //TODO: this signals should connect to csr
  fence.disableSfence := DontCare
}
