package exu
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import common.{ExuOutput, FuType, Redirect, XSBundle, XSParam}
import fu.FuConfigs
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
  override lazy val module = new JmpCsrExuImpl(this)
}
class JmpCsrExuImpl(outer:JmpCsrExu)(implicit p:Parameters) extends BasicExuImpl(outer){
  val io = IO(new Bundle{
    val bypassIn = Input(Vec(outer.bypassInNum, Valid(new ExuOutput)))
    val fenceio = new FenceIO
  })
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1
  private val fence = Module(new Fence)
  private val jmp = Module(new Jump)
  private val i2f = Module(new IntToFP)

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

  private val fuList = Seq(fence, jmp, i2f)
  fuList.foreach(fu => {
    fu.io.redirectIn := redirectIn
    fu.io.in.valid := finalIssueSignals.valid
    fu.io.in.bits.uop := finalIssueSignals.bits.uop
    fu.io.in.bits.src := finalIssueSignals.bits.src
  })
  private val outValidVec = fuList.map(_.io.out.valid)
  private val outDataVec = fuList.map(_.io.out.bits)
  private val outData = Mux1H(outValidVec, outDataVec)
  writebackPort.valid := Cat(outValidVec).orR
  writebackPort.bits.uop := outData.uop
  writebackPort.bits.data := outData.data

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
