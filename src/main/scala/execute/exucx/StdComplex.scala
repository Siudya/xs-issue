package execute.exucx

import chipsalliance.rocketchip.config.Parameters
import chisel3.{Bundle, DontCare, Input}
import chisel3.util.Valid
import common.Redirect
import execute.exu.StdExu
import freechips.rocketchip.diplomacy.LazyModule

class StdComplex(id: Int)(implicit p:Parameters) extends BasicExuComplex{
  val bypassNum = 0
  val std = LazyModule(new StdExu(id,"StdComplex", bypassNum))
  std.issueNode :*= issueNode
  writebackNode :=* std.writebackNode
  lazy val module = new BasicExuComplexImp(this, bypassNum){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 1)
    private val issueIn = issueNode.in.head._1
    private val issueRouted = issueNode.out.map(_._1)
    issueRouted.foreach(_ <> issueIn)
    std.module.redirectIn := redirectIn
    std.module.io.bypassIn := bypassIn
    issueIn.fuInFire := DontCare
  }
}
