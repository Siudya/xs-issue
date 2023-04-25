package execute.exu
import chisel3.util._
import chisel3._
import chipsalliance.rocketchip.config.Parameters
import common.{ExuInput, ExuOutput, MicroOp, Redirect}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import issue.IssueBundle
import xs.utils.Assertion.xs_assert

abstract class BasicExu(implicit p:Parameters) extends LazyModule{
  def issueNode: ExuInputNode
  def writebackNode: ExuOutNode
  def module:BasicExuImpl
}

abstract class BasicExuImpl(outer:BasicExu) extends LazyModuleImp(outer){
  val redirectIn = IO(Input(Valid(new Redirect)))

  def bypassSigGen(bypassIn:Seq[Valid[ExuOutput]], issuePort:IssueBundle, hasBypass:Boolean):Valid[ExuInput] = {
    val finalIssueSignals = Wire(Valid(new ExuInput))
    finalIssueSignals.valid := issuePort.issue.valid
    finalIssueSignals.bits.uop := issuePort.issue.bits.uop
    finalIssueSignals.bits.src := issuePort.issue.bits.src
    if(hasBypass) {
      val bypass = bypassIn
      val bypassData = bypass.map(_.bits.data)
      val bypassSrc0Hits = bypass.map(elm => elm.valid && elm.bits.uop.pdest === issuePort.issue.bits.uop.psrc(0))
      val bypassSrc1Hits = bypass.map(elm => elm.valid && elm.bits.uop.pdest === issuePort.issue.bits.uop.psrc(1))
      val bypassSrc0Valid = Cat(bypassSrc0Hits).orR
      val bypassSrc0Data = Mux1H(bypassSrc0Hits, bypassData)
      val bypassSrc1Valid = Cat(bypassSrc1Hits).orR
      val bypassSrc1Data = Mux1H(bypassSrc1Hits, bypassData)
      xs_assert(PopCount(Cat(bypassSrc0Hits)) === 1.U || Cat(bypassSrc0Hits) === 0.U)
      xs_assert(PopCount(Cat(bypassSrc1Hits)) === 1.U || Cat(bypassSrc1Hits) === 0.U)
      finalIssueSignals.bits.src(0) := Mux(bypassSrc0Valid, bypassSrc0Data, issuePort.issue.bits.src(0))
      finalIssueSignals.bits.src(1) := Mux(bypassSrc1Valid, bypassSrc1Data, issuePort.issue.bits.src(1))
    }
    finalIssueSignals
  }
}
