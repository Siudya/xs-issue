package execute.exu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import common.{ExuOutput, XSParam}
import execute.fu.FuConfigs
import execute.fu.fpu.FDivSqrt
import xs.utils.Assertion.xs_assert
import xs.utils.PickOneHigh

class FdivExu(id:Int, complexName:String)(implicit p:Parameters) extends BasicExu with XSParam{
  private val cfg = ExuConfig(
    name = "FdivExu",
    id = id,
    complexName = complexName,
    fuConfigs = Seq(FuConfigs.fdivSqrtCfg, FuConfigs.fdivSqrtCfg, FuConfigs.fdivSqrtCfg),
    exuType = ExuType.fdiv,
    needToken = true
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutputNode(cfg)
  lazy val module = new FdivExuImpl(this, cfg)
}
class FdivExuImpl(outer:FdivExu, exuCfg:ExuConfig)(implicit p:Parameters) extends BasicExuImpl(outer) with XSParam{
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1

  private val fdivSqrts = Seq.fill(exuCfg.fuConfigs.length)(Module(new FDivSqrt))
  private val outputArbiter = Module(new Arbiter(new ExuOutput, exuCfg.fuConfigs.length))

  private val fuSel = PickOneHigh(Cat(fdivSqrts.map(_.io.in.ready).reverse))
  issuePort.issue.ready := fuSel.valid
  issuePort.fmaMidState.out := DontCare
  issuePort.fuInFire := DontCare
  fdivSqrts.zipWithIndex.zip(outputArbiter.io.in).foreach({case((fu,idx), arbIn) =>
    fu.io.redirectIn := redirectIn
    fu.io.in.valid := issuePort.issue.valid & fuSel.bits(idx) & issuePort.issue.bits.uop.ctrl.fuType === exuCfg.fuConfigs.head.fuType
    fu.io.in.bits.uop := issuePort.issue.bits.uop
    fu.io.in.bits.src := issuePort.issue.bits.src
    fu.rm := issuePort.issue.bits.uop.ctrl.fpu.rm
    fu.io.out.ready := arbIn.ready
    arbIn.valid := fu.io.out.valid
    arbIn.bits.data := fu.io.out.bits.data
    arbIn.bits.uop := fu.io.out.bits.uop
    arbIn.bits.fflags := fu.fflags
    arbIn.bits.redirect := DontCare
    arbIn.bits.redirectValid := false.B
  })
  xs_assert(Mux(issuePort.issue.valid, fuSel.valid, true.B))
  writebackPort.valid := outputArbiter.io.out.valid
  writebackPort.bits := outputArbiter.io.out.bits
  outputArbiter.io.out.ready := true.B
}
