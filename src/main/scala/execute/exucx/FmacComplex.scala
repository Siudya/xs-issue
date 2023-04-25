package execute.exucx

import chipsalliance.rocketchip.config.Parameters
import chisel3.{Bundle, DontCare, Input}
import chisel3.util.Valid
import common.Redirect
import execute.exu.FmacExu
import freechips.rocketchip.diplomacy.LazyModule

class FmacComplex(id: Int)(implicit p:Parameters) extends BasicExuComplex{
  val fmac = LazyModule(new FmacExu(id,"FmacComplex"))
  fmac.issueNode :*= issueNode
  writebackNode :=* fmac.writebackNode
  lazy val module = new BasicExuComplexImp(this){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 1)
    private val issueIn = issueNode.in.head._1
    private val issueRouted = issueNode.out.map(_._1)
    issueRouted.foreach(_ <> issueIn)
    fmac.module.redirectIn := redirectIn
    issueIn.fuInFire := DontCare
  }
}
