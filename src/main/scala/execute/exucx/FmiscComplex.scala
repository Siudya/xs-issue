package execute.exucx

import chipsalliance.rocketchip.config.Parameters
import chisel3.{Bundle, Input}
import chisel3.util.Valid
import common.Redirect
import exu.FmiscExu
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

class FmiscComplex(id: Int)(implicit p:Parameters) extends LazyModule{
  val issueNode = new ExuComplexIssueNode
  val writebackNode = new ExuComplexWritebackNode
  val fmisc = new FmiscExu(id)
  fmisc.issueNode :*= issueNode
  writebackNode :=* fmisc.writebackNode
  lazy val module = new LazyModuleImp(this){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 1)
    val io = IO(new Bundle{
      val redirect = Input(Valid(new Redirect))
    })
    private val issueIn = issueNode.in.head._1
    private val issueRouted = issueNode.out.map(_._1)
    issueRouted.foreach(_ <> issueIn)
    fmisc.module.redirectIn := io.redirect
  }
}
