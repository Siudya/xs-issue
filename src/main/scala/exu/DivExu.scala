package exu
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import common.{ExuOutput, Redirect, XSParam}
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

  private val finalIssueSignals = WireInit(issuePort.fuInput)
  if (outer.bypassInNum > 0) {
    val bypass = io.bypassIn
    val bypassData = bypass.map(_.bits.data)
    val bypassSrc0Hits = bypass.map(elm => elm.valid && elm.bits.uop.pdest === issuePort.fuInput.bits.uop.psrc(0))
    val bypassSrc1Hits = bypass.map(elm => elm.valid && elm.bits.uop.pdest === issuePort.fuInput.bits.uop.psrc(1))
    val bypassSrc0Valid = Cat(bypassSrc0Hits).orR
    val bypassSrc0Data = Mux1H(bypassSrc0Hits, bypassData)
    val bypassSrc1Valid = Cat(bypassSrc1Hits).orR
    val bypassSrc1Data = Mux1H(bypassSrc1Hits, bypassData)
    finalIssueSignals.bits.src(0) := Mux(bypassSrc0Valid, bypassSrc0Data, issuePort.fuInput.bits.src(0))
    finalIssueSignals.bits.src(1) := Mux(bypassSrc1Valid, bypassSrc1Data, issuePort.fuInput.bits.src(1))
  }

  private val releaseDriverRegs = RegInit(VecInit(Seq.fill(divNumInOneExu)(false.B)))
  private val divSel = finalIssueSignals.bits.uop.fuSel
  xs_assert(Mux(finalIssueSignals.valid, PopCount(divSel) === 1.U, true.B))
  for((((div, en), arbIn), rlsReg) <- divs.zip(divSel.asBools).zip(outputArbiter.io.in).zip(releaseDriverRegs)){
    div.io.redirectIn := redirectIn
    div.io.in.valid := finalIssueSignals.valid & en
    div.io.in.bits.uop := finalIssueSignals.bits.uop
    div.io.in.bits.src := finalIssueSignals.bits.src
    arbIn <> div.io.out
    when(div.io.out.fire){
      rlsReg := true.B
    }.elsewhen(rlsReg === true.B){
      rlsReg := false.B
    }
    xs_assert(Mux(div.io.in.valid, div.io.in.ready, true.B))
  }
  outputArbiter.io.out.ready := true.B
  writebackPort.bits := DontCare
  writebackPort.valid := outputArbiter.io.out.valid
  writebackPort.bits.uop := outputArbiter.io.out.bits.uop
  writebackPort.bits.data := outputArbiter.io.out.bits.data
  issuePort.release := Cat(releaseDriverRegs.reverse)
}