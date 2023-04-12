package exu
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import common.{ExuOutput, Redirect}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import fu.FuConfigs
import fu.alu.Alu
import xs.utils.Assertion.xs_assert

class AluExu(id:Int, val bypassInNum:Int)(implicit p:Parameters) extends BasicExu{
  private val cfg  = ExuConfig(
    name = "AluExu",
    id = id,
    blockName = "IntegerBlock",
    fuConfigs = Seq(FuConfigs.aluCfg),
    exuType = ExuType.alu,
    srcNum = 2
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutNode(cfg)

  lazy val module = new AluExuImpl(this)
}
class AluExuImpl(outer:AluExu)(implicit p:Parameters) extends BasicExuImpl(outer){
  val io = IO(new Bundle{
    val redirectOut = Output(Valid(new Redirect))
    val bypassIn = Input(Vec(outer.bypassInNum, Valid(new ExuOutput))) //Alu does not need bypass out for its latency is 0. Bypassing in regfile is enough.
  })
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1

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
    xs_assert(PopCount(Cat(bypassSrc0Hits)) === 1.U || Cat(bypassSrc0Hits) === 0.U)
    xs_assert(PopCount(Cat(bypassSrc1Hits)) === 1.U || Cat(bypassSrc1Hits) === 0.U)
    finalIssueSignals.bits.src(0) := Mux(bypassSrc0Valid, bypassSrc0Data, issuePort.fuInput.bits.src(0))
    finalIssueSignals.bits.src(1) := Mux(bypassSrc1Valid, bypassSrc1Data, issuePort.fuInput.bits.src(1))
  }

  private val alu = Module(new Alu)
  alu.io.in.valid := finalIssueSignals.valid
  alu.io.in.bits.uop := finalIssueSignals.bits.uop
  alu.io.in.bits.src := finalIssueSignals.bits.src

  writebackPort := DontCare
  writebackPort.valid := alu.io.out.valid
  writebackPort.bits.uop := alu.io.out.bits.uop
  writebackPort.bits.data := alu.io.out.bits.data
  alu.io.redirectIn := redirectIn
  io.redirectOut.valid := alu.redirectOutValid
  io.redirectOut.bits := alu.redirectOut
}
