package exu
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import common.XSParam
import fu.FuConfigs
import fu.fpu.FMA

class FmacExu(id :Int)(implicit p:Parameters) extends BasicExu with XSParam{
  private val cfg = ExuConfig(
    name = "FmacExu",
    id = id,
    blockName = "FloatingBlock",
    fuConfigs = Seq(FuConfigs.fmacCfg),
    exuType = ExuType.fmac,
    speculativeWakeup = true
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutNode(cfg)
  lazy val module = new FmacExuImpl(this, cfg)
}
class FmacExuImpl(outer:FmacExu, exuCfg:ExuConfig)(implicit p:Parameters) extends BasicExuImpl(outer){
  private val fmac = Module(new FMA)
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1

  fmac.io.redirectIn := redirectIn
  fmac.io.in.valid := issuePort.issue.valid && issuePort.issue.bits.uop.ctrl.fuType === exuCfg.fuConfigs.head.fuType
  fmac.io.in.bits.uop := issuePort.issue.bits.uop
  fmac.io.in.bits.src := issuePort.issue.bits.src
  issuePort.issue.ready := fmac.io.in.ready
  fmac.rm := issuePort.issue.bits.uop.ctrl.fpu.rm
  fmac.midResult <> issuePort.fmaMidState

  writebackPort.valid := fmac.io.out.valid
  fmac.io.out.ready := true.B
  writebackPort.bits.uop := fmac.io.out.bits.uop
  writebackPort.bits.data := fmac.io.out.bits.data
  writebackPort.bits.fflags := fmac.fflags
  writebackPort.bits.redirect := DontCare
  writebackPort.bits.redirectValid := false.B
}
