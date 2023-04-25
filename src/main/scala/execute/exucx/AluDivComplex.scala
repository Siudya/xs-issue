package execute.exucx
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.Valid
import common.{ExuOutput, FuType}
import execute.exu.{AluExu, DivExu, ExuType}
import freechips.rocketchip.diplomacy.LazyModule
import xs.utils.Assertion.xs_assert

class AluDivComplex(id: Int, bypassNum:Int)(implicit p:Parameters) extends BasicExuComplex{
  val alu = LazyModule(new AluExu(id, "AluDivComplex", bypassNum))
  val div = LazyModule(new DivExu(id, "AluDivComplex", bypassNum))
  alu.issueNode :*= issueNode
  div.issueNode :*= issueNode
  writebackNode :=* alu.writebackNode
  writebackNode :=* div.writebackNode
  lazy val module = new BasicExuComplexImp(this){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 2)
    val io = IO(new Bundle{
      val bypassIn = Input(Vec(bypassNum, Valid(new ExuOutput)))
    })
    private val issueIn = issueNode.in.head._1
    private val issueAlu = issueNode.out.filter(_._2.exuType == ExuType.alu).head._1
    private val issueDiv = issueNode.out.filter(_._2.exuType == ExuType.div).head._1
    val aa = issueNode.out

    issueAlu <> issueIn
    alu.module.io.bypassIn := io.bypassIn
    alu.module.redirectIn := redirectIn

    issueDiv <> issueIn
    div.module.io.bypassIn := io.bypassIn
    div.module.redirectIn := redirectIn

    issueIn.issue.ready := Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.alu, issueAlu.issue.ready, issueDiv.issue.ready)
    issueIn.fuInFire := DontCare

    private val issueFuHit = issueNode.in.head._2.exuConfigs.flatMap(_.fuConfigs).map(_.fuType === issueIn.issue.bits.uop.ctrl.fuType).reduce(_|_)
    xs_assert(Mux(issueIn.issue.valid, issueFuHit, true.B))
  }
}
