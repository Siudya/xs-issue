package exu
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import common.XSParam
import fu.FuConfigs
import fu.fpu.{FPToFP, FPToInt}
import xs.utils.Assertion.xs_assert

class FmiscExu(id:Int, complexName:String)(implicit p:Parameters) extends BasicExu{
  private val cfg = ExuConfig(
    name = "FmiscExu",
    id = id,
    complexName = complexName,
    fuConfigs = Seq(FuConfigs.f2iCfg, FuConfigs.f2fCfg),
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

  private val fuList = Seq(f2i, f2f)
  issuePort.issue.ready := true.B
  issuePort.fmaMidState.out.valid := false.B
  issuePort.fmaMidState.out.bits := DontCare
  issuePort.fuInFire := DontCare
  fuList.zip(exuCfg.fuConfigs).foreach({case(fu,cfg) =>
    fu.io.redirectIn := redirectIn
    fu.rm := issuePort.issue.bits.uop.ctrl.fpu.rm
    fu.io.in.valid := issuePort.issue.valid & issuePort.issue.bits.uop.ctrl.fuType === cfg.fuType
    fu.io.in.bits.uop := issuePort.issue.bits.uop
    fu.io.in.bits.src := issuePort.issue.bits.src
    fu.io.out.ready := true.B
  })

  xs_assert(Mux(issuePort.issue.valid, exuCfg.fuConfigs.map(_.fuType === issuePort.issue.bits.uop.ctrl.fuType).reduce(_|_), true.B))
  //This module should never be blocked.
  xs_assert(Mux(f2i.io.in.valid, f2i.io.in.ready, true.B))
  xs_assert(Mux(f2f.io.in.valid, f2f.io.in.ready, true.B))

  private val valids = fuList.map(_.io.out.valid)
  private val uops = fuList.map(_.io.out.bits.uop)
  private val data = fuList.map(_.io.out.bits.data)
  private val fflags = fuList.map(_.fflags)
  writebackPort.valid := valids.reduce(_|_)
  writebackPort.bits.uop := Mux1H(valids, uops)
  writebackPort.bits.data := Mux1H(valids, data)
  writebackPort.bits.fflags := Mux1H(valids, fflags)
  writebackPort.bits.redirect := DontCare
  writebackPort.bits.redirectValid := false.B
}
