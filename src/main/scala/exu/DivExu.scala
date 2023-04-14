package exu
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import common.{ExuInput, ExuOutput, Redirect, XSParam}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import fu.{FuConfigs, FuOutput}
import fu.alu.Alu
import fu.mdu.DividerWrapper
import xs.utils.Assertion.xs_assert

class DivExu(id:Int, val bypassInNum:Int)(implicit p:Parameters) extends BasicExu with XSParam{
  private val cfg = ExuConfig(
    name = "DivExu",
    id = id,
    blockName = "IntegerBlock",
    fuConfigs = Seq.fill(divNumInOneExu)(FuConfigs.divCfg),
    exuType = ExuType.div,
    releaseWidth = divNumInOneExu
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutNode(cfg)
  lazy val module = new DivExuImpl(this)
}

class DivExuImpl(outer:DivExu) extends BasicExuImpl(outer) with XSParam{
  val io = IO(new Bundle {
    val bypassIn = Input(Vec(outer.bypassInNum, Valid(new ExuOutput)))
  })
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1
  private val divs = Seq.fill(divNumInOneExu)(Module(new DividerWrapper(XLEN)))
  private val outputArbiter = Module(new Arbiter(new FuOutput(XLEN), divNumInOneExu))

  issuePort.feedback.ready := true.B
  private val finalIssueSignals = bypassSigGen(io.bypassIn, issuePort, outer.bypassInNum > 0)

  private val releaseDrivers = Wire(UInt(divNumInOneExu.W))
  private val divSel = finalIssueSignals.bits.uop.fuSel
  xs_assert(Mux(finalIssueSignals.valid, PopCount(divSel) === 1.U, true.B))
  for((((div, en), arbIn), rls) <- divs.zip(divSel.asBools).zip(outputArbiter.io.in).zip(releaseDrivers.asBools)){
    div.io.redirectIn := redirectIn
    div.io.in.valid := finalIssueSignals.valid & en
    div.io.in.bits.uop := finalIssueSignals.bits.uop
    div.io.in.bits.src := finalIssueSignals.bits.src
    arbIn <> div.io.out
    rls := div.io.out.fire
    xs_assert(Mux(div.io.in.valid, div.io.in.ready, true.B))
  }
  outputArbiter.io.out.ready := true.B
  writebackPort.bits := DontCare
  writebackPort.valid := outputArbiter.io.out.valid
  writebackPort.bits.uop := outputArbiter.io.out.bits.uop
  writebackPort.bits.data := outputArbiter.io.out.bits.data
  issuePort.feedback.release := releaseDrivers
}