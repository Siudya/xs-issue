package execute.exucx
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.Valid
import common.{ExuOutput, FuType, Redirect}
import exu.{AluExu, DivExu, ExuType}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xs.utils.Assertion.xs_assert

class AluDivComplex(id: Int, bypassNum:Int)(implicit p:Parameters) extends LazyModule{
  val issueNode = new ExuComplexIssueNode
  val writebackNode = new ExuComplexWritebackNode
  val alu = new AluExu(id, bypassNum)
  val div = new DivExu(id, bypassNum)
  alu.issueNode :*= issueNode
  div.issueNode :*= issueNode
  writebackNode :=* alu.writebackNode
  writebackNode :=* div.writebackNode
  lazy val module = new LazyModuleImp(this){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 2)
    val io = IO(new Bundle{
      val redirect = Input(Valid(new Redirect))
      val bypassIn = Input(Vec(bypassNum, Valid(new ExuOutput)))
    })
    private val issueIn = issueNode.in.head._1
    private val issueAlu = issueNode.out.filter(_._2.exuType == ExuType.alu).head._1
    private val issueDiv = issueNode.out.filter(_._2.exuType == ExuType.div).head._1

    issueAlu <> issueIn
    alu.module.io.bypassIn := io.bypassIn
    alu.module.redirectIn := io.redirect

    issueDiv <> issueIn
    div.module.io.bypassIn := io.bypassIn
    div.module.redirectIn := io.redirect

    issueIn.issue.ready := Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.alu, issueAlu.issue.ready, issueDiv.issue.ready)
    issueIn.fuInFire := DontCare

    private val issueFuHit = issueNode.in.head._2.exuConfigs.flatMap(_.fuConfigs).map(_.fuType === issueIn.issue.bits.uop.ctrl.fuType).reduce(_|_)
    xs_assert(Mux(issueIn.issue.valid, issueFuHit, true.B))
  }
}
