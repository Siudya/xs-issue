package execute.exu

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import common.{ExuInput, ExuOutput, FuType, Redirect}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import execute.fu.FuConfigs
import execute.fu.fpu.IntToFP
import xs.utils.Assertion.xs_assert

class I2fExu(id:Int, complexName:String, val bypassInNum:Int)(implicit p:Parameters) extends BasicExu{
  private val cfg  = ExuConfig(
    name = "I2fExu",
    id = id,
    complexName = complexName,
    fuConfigs = Seq(FuConfigs.i2fCfg),
    exuType = ExuType.i2f
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutputNode(cfg)

  lazy val module = new I2fImpl(this, cfg)
}
class I2fImpl(outer:I2fExu, exuCfg:ExuConfig)(implicit p:Parameters) extends BasicExuImpl(outer){
  val io = IO(new Bundle{
    val bypassIn = Input(Vec(outer.bypassInNum, Valid(new ExuOutput))) //Alu does not need bypass out for its latency is 0. Bypassing in regfile is enough.
  })
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1

  issuePort.issue.ready := true.B
  issuePort.fuInFire := DontCare
  issuePort.fmaMidState.out := DontCare
  private val finalIssueSignals = bypassSigGen(io.bypassIn, issuePort, outer.bypassInNum > 0)

  private val i2f = Module(new IntToFP)
  i2f.rm := finalIssueSignals.bits.uop.ctrl.fpu.rm
  i2f.io.redirectIn := redirectIn
  i2f.io.in.valid := finalIssueSignals.valid && finalIssueSignals.bits.uop.ctrl.fuType === exuCfg.fuConfigs.head.fuType
  i2f.io.in.bits.uop := finalIssueSignals.bits.uop
  i2f.io.in.bits.src := finalIssueSignals.bits.src
  i2f.io.out.ready := true.B

  writebackPort := DontCare
  writebackPort.valid := i2f.io.out.valid
  writebackPort.bits.uop := i2f.io.out.bits.uop
  writebackPort.bits.data := i2f.io.out.bits.data
  writebackPort.bits.fflags := i2f.fflags
  xs_assert(i2f.io.in.ready)
}
