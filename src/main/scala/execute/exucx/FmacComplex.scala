package execute.exucx

import chipsalliance.rocketchip.config.Parameters
import chisel3.{Bundle, DontCare, Input}
import chisel3.util.Valid
import common.Redirect
import exu.FmacExu
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

class FmacComplex(id: Int)(implicit p:Parameters) extends LazyModule{
  val issueNode = new ExuComplexIssueNode
  val writebackNode = new ExuComplexWritebackNode
  val fmac = new FmacExu(id,"FmacComplex")
  fmac.issueNode :*= issueNode
  writebackNode :=* fmac.writebackNode
  lazy val module = new LazyModuleImp(this){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 1)
    val io = IO(new Bundle{
      val redirect = Input(Valid(new Redirect))
    })
    private val issueIn = issueNode.in.head._1
    private val issueRouted = issueNode.out.map(_._1)
    issueRouted.foreach(_ <> issueIn)
    fmac.module.redirectIn := io.redirect
    issueIn.fuInFire := DontCare
  }
}
