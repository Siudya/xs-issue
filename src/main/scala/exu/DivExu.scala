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
import xs.utils.PickOneHigh

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

  private val finalIssueSignals = bypassSigGen(io.bypassIn, issuePort, outer.bypassInNum > 0)

  private val divSel = PickOneHigh(Cat(divs.map(_.io.in.ready).reverse))
  issuePort.issue.ready := divSel.valid
  for(((div, en), arbIn) <- divs.zip(Mux(divSel.valid, divSel.bits, 0.U).asBools).zip(outputArbiter.io.in)){
    div.io.redirectIn := redirectIn
    div.io.in.valid := finalIssueSignals.valid & en
    div.io.in.bits.uop := finalIssueSignals.bits.uop
    div.io.in.bits.src := finalIssueSignals.bits.src
    arbIn <> div.io.out
    xs_assert(Mux(div.io.in.valid, div.io.in.ready, true.B))
  }
  outputArbiter.io.out.ready := true.B
  writebackPort.bits := DontCare
  writebackPort.valid := outputArbiter.io.out.valid
  writebackPort.bits.uop := outputArbiter.io.out.bits.uop
  writebackPort.bits.data := outputArbiter.io.out.bits.data
}