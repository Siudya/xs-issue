package execute.exucx

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.Valid
import common.{ExuOutput, FuType, Redirect}
import execute.exu.{AluExu, ExuType, MulExu}
import freechips.rocketchip.diplomacy.LazyModule
import xs.utils.Assertion.xs_assert

class AluMulComplex(id: Int, bypassNum:Int)(implicit p:Parameters) extends BasicExuComplex{
  val alu = LazyModule(new AluExu(id, "AluMulComplex", bypassNum + 1))
  val mul = LazyModule(new MulExu(id, "AluMulComplex", bypassNum))
  alu.issueNode :*= issueNode
  mul.issueNode :*= issueNode
  writebackNode :=* alu.writebackNode
  writebackNode :=* mul.writebackNode

  lazy val module = new BasicExuComplexImp(this, bypassNum){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 2)
    val io = IO(new Bundle{
      val bypassOut = Output(Valid(new ExuOutput))
    })
    private val issueIn = issueNode.in.head._1
    private val issueAlu = issueNode.out.filter(_._2._2.exuType == ExuType.alu).head._1
    private val issueMul = issueNode.out.filter(_._2._2.exuType == ExuType.mul).head._1

    issueAlu <> issueIn
    alu.module.io.bypassIn.take(bypassNum).zip(bypassIn).foreach({case(a, b) => a := b})
    alu.module.redirectIn := redirectIn

    issueMul <> issueIn
    mul.module.io.bypassIn := bypassIn
    mul.module.redirectIn := redirectIn

    alu.module.io.bypassIn.last := mul.module.io.bypassOut
    io.bypassOut := mul.module.io.bypassOut

    issueIn.fuInFire := DontCare
    issueIn.issue.ready := Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.alu, issueAlu.issue.ready, issueMul.issue.ready)
    private val issueFuHit = issueNode.in.head._2._2.exuConfigs.flatMap(_.fuConfigs).map(_.fuType === issueIn.issue.bits.uop.ctrl.fuType).reduce(_ | _)
    xs_assert(Mux(issueIn.issue.valid, issueFuHit, true.B))
  }
}
