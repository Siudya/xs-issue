package execute.exucx

import chipsalliance.rocketchip.config.Parameters
import chisel3.{Bundle, DontCare, Input}
import chisel3.util.Valid
import common.Redirect
import execute.exu.StaExu
import freechips.rocketchip.diplomacy.LazyModule

class StaComplex(id: Int)(implicit p:Parameters) extends BasicExuComplex{
  val bypassNum = 0
  val sta = LazyModule(new StaExu(id,"LduComplex", bypassNum))
  sta.issueNode :*= issueNode
  writebackNode :=* sta.writebackNode
  lazy val module = new BasicExuComplexImp(this, bypassNum){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 1)
    private val issueIn = issueNode.in.head._1
    private val issueRouted = issueNode.out.map(_._1)
    issueRouted.foreach(_ <> issueIn)
    sta.module.redirectIn := redirectIn
    sta.module.io.bypassIn := bypassIn
    issueIn.fuInFire := DontCare
  }
}
