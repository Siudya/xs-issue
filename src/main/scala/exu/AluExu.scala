package exu
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import common.{ExuOutput, Redirect}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import fu.FuConfigs
import fu.alu.Alu
import xs.utils.Assertion.xs_assert

class AluExu(id:Int, val bypassInNum:Int)(implicit p:Parameters) extends LazyModule{
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
class AluExuImpl(outer:AluExu)(implicit p:Parameters) extends LazyModuleImp(outer){
  val io = IO(new Bundle{
    val redirectIn = Input(Valid(new Redirect))
    val redirectOut = Output(Valid(new Redirect))
    val bypassIn = Input(Vec(outer.bypassInNum, Valid(new ExuOutput))) //Alu does not need bypass out for its latency is 0. Bypassing in regfile is enough.
  })
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1
  private val bypassData = io.bypassIn.map(_.bits.data)
  private val bypassSrc0Hits = io.bypassIn.map(elm => elm.valid && elm.bits.uop.pdest === issuePort.fuInput.bits.uop.psrc(0))
  private val bypassSrc1Hits = io.bypassIn.map(elm => elm.valid && elm.bits.uop.pdest === issuePort.fuInput.bits.uop.psrc(1))
  private val bypassSrc0Valid = Cat(bypassSrc0Hits).orR
  private val bypassSrc0Data = Mux1H(bypassSrc0Hits, bypassData)
  private val bypassSrc1Valid = Cat(bypassSrc1Hits).orR
  private val bypassSrc1Data = Mux1H(bypassSrc1Hits, bypassData)
  private val alu = Module(new Alu)
  alu.io.in.valid := issuePort.fuInput.valid
  alu.io.in.bits.uop := issuePort.fuInput.bits.uop
  alu.io.in.bits.src(0) := Mux(bypassSrc0Valid, bypassSrc0Data, issuePort.fuInput.bits.src(0))
  alu.io.in.bits.src(1) := Mux(bypassSrc1Valid, bypassSrc1Data, issuePort.fuInput.bits.src(1))
  alu.io.in.bits.src(2) := DontCare

  writebackPort := DontCare
  writebackPort.valid := alu.io.out.valid
  writebackPort.bits.uop := alu.io.out.bits.uop
  writebackPort.bits.data := alu.io.out.bits.data
  alu.io.redirectIn := io.redirectIn
  io.redirectOut.valid := alu.redirectOutValid
  io.redirectOut.bits := alu.redirectOut

  xs_assert(PopCount(Cat(bypassSrc0Hits)) === 1.U || Cat(bypassSrc0Hits) === 0.U)
  xs_assert(PopCount(Cat(bypassSrc1Hits)) === 1.U || Cat(bypassSrc1Hits) === 0.U)
}
