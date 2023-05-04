package execute.exu

import chipsalliance.rocketchip.config.Parameters
import chisel3.{Bundle, DontCare, Input, Vec}
import chisel3.util.Valid
import common.ExuOutput
import execute.fu.FuConfigs

class StdMouExu(id:Int, complexName:String, val bypassInNum:Int)(implicit p: Parameters) extends BasicExu {
  private val cfg = ExuConfig(
    name = "StdMouExu",
    id = id,
    complexName = complexName,
    fuConfigs = Seq(FuConfigs.stdCfg, FuConfigs.mouCfg, FuConfigs.mouDataCfg),
    exuType = ExuType.mou
  )
  lazy val module = new StdMouExuImp(this, cfg)
  val issueNode: ExuInputNode = new ExuInputNode(cfg)
  val writebackNode: ExuOutNode = new ExuOutNode(cfg)
}

class StdMouExuImp(outer:StdMouExu, exuCfg:ExuConfig) extends BasicExuImpl(outer){
  val io = IO(new Bundle {
    val bypassIn = Input(Vec(outer.bypassInNum, Valid(new ExuOutput)))
  })
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1

  private val finalIssueSignals = bypassSigGen(io.bypassIn, issuePort, outer.bypassInNum > 0)
  private val redirect = redirectIn
  issuePort := DontCare
  writebackPort := DontCare
}