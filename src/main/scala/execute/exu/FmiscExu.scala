package exu
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import common.{ExuOutput, XSParam}
import fu.{FuConfigs, FuOutput}
import fu.fpu.{FDivSqrt, FPToFP, FPToInt}
import xs.utils.Assertion.xs_assert

class FmiscExu(id :Int)(implicit p:Parameters) extends BasicExu{
  private val cfg = ExuConfig(
    name = "FmiscExu",
    id = id,
    blockName = "FloatingBlock",
    fuConfigs = Seq(FuConfigs.f2iCfg, FuConfigs.f2fCfg, FuConfigs.fdivSqrtCfg),
    exuType = ExuType.fmisc
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutNode(cfg)
  lazy val module = new FmiscExuImpl(this, cfg)
}
class FmiscExuImpl(outer:FmiscExu, exuCfg:ExuConfig)(implicit p:Parameters) extends BasicExuImpl(outer) with XSParam{
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1

  private val f2i = Module(new FPToInt)
  private val f2f = Module(new FPToFP)
  private val fdivSqrt = Module(new FDivSqrt)
  private val outputArbiter = Module(new Arbiter(new ExuOutput, exuCfg.fuConfigs.length))

  private val fuList = Seq(f2i, f2f, fdivSqrt)
  private val fuReadies = exuCfg.fuConfigs.zip(fuList).zip(outputArbiter.io.in).map({ case ((cfg, fu), arbIn) =>
    val fuHit = issuePort.issue.bits.uop.ctrl.fuType === cfg.fuType
    fu.io.redirectIn := redirectIn
    fu.rm := issuePort.issue.bits.uop.ctrl.fpu.rm
    fu.io.in.valid := issuePort.issue.valid & fuHit
    fu.io.in.bits.uop := issuePort.issue.bits.uop
    fu.io.in.bits.src := issuePort.issue.bits.src
    fu.io.out.ready := arbIn.ready
    arbIn.valid := fu.io.out.valid
    arbIn.bits.uop := fu.io.out.bits.uop
    arbIn.bits.data := fu.io.out.bits.data
    arbIn.bits.fflags := fu.fflags
    arbIn.bits.redirect := DontCare
    arbIn.bits.redirectValid := false.B
    fuHit && fu.io.in.ready
  })
  outputArbiter.io.out.ready := true.B
  private val inFuHits = outer.cfg.fuConfigs.map({ cfg => issuePort.issue.bits.uop.ctrl.fuType === cfg.fuType })
  xs_assert(Mux(issuePort.issue.valid, PopCount(Cat(inFuHits)) === 1.U, true.B))
  issuePort.issue.ready := Mux1H(inFuHits, fuReadies)

  writebackPort.valid := outputArbiter.io.out.valid
  writebackPort.bits := outputArbiter.io.out.bits
}
