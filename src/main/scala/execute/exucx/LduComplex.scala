package execute.exucx

import chipsalliance.rocketchip.config.Parameters
import chisel3.{Bundle, DontCare, Input}
import chisel3.util.Valid
import common.Redirect
import execute.exu.LduExu
import freechips.rocketchip.diplomacy.LazyModule

class LduComplex(id: Int, bypassNum:Int)(implicit p:Parameters) extends BasicExuComplex{
  val ldu = LazyModule(new LduExu(id,"LduComplex", bypassNum))
  ldu.issueNode :*= issueNode
  writebackNode :=* ldu.writebackNode
  lazy val module = new BasicExuComplexImp(this, bypassNum){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 1)
    private val issueIn = issueNode.in.head._1
    private val issueRouted = issueNode.out.map(_._1)
    issueRouted.foreach(_ <> issueIn)
    ldu.module.redirectIn := redirectIn
    ldu.module.io.bypassIn := bypassIn
    issueIn.fuInFire := DontCare
  }
}
