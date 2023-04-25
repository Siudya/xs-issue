package execute.exucx
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import common.{ExuOutput, FuType}
import execute.exu.{AluExu, ExuType, I2fExu}
import freechips.rocketchip.diplomacy.LazyModule
import xs.utils.Assertion.xs_assert

class AluI2FComplex(id: Int, bypassNum:Int)(implicit p:Parameters) extends BasicExuComplex{
  val alu = LazyModule(new AluExu(id, "AluI2FComplex", bypassNum))
  val i2f = LazyModule(new I2fExu(id, "AluI2FComplex", bypassNum))
  alu.issueNode :*= issueNode
  i2f.issueNode :*= issueNode
  writebackNode :=* alu.writebackNode
  writebackNode :=* i2f.writebackNode
  lazy val module = new BasicExuComplexImp(this){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 2)
    val io = IO(new Bundle{
      val bypassIn = Input(Vec(bypassNum, Valid(new ExuOutput)))
    })
    private val issueIn = issueNode.in.head._1
    private val issueAlu = issueNode.out.filter(_._2.exuType == ExuType.alu).head._1
    private val issueI2f = issueNode.out.filter(_._2.exuType == ExuType.i2f).head._1

    issueAlu <> issueIn
    alu.module.io.bypassIn := io.bypassIn
    alu.module.redirectIn := redirectIn

    issueI2f <> issueIn
    i2f.module.io.bypassIn := io.bypassIn
    i2f.module.redirectIn := redirectIn

    issueIn.fuInFire := DontCare
    issueIn.issue.ready := Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.alu, issueAlu.issue.ready, issueI2f.issue.ready)
    private val issueFuHit = issueNode.in.head._2.exuConfigs.flatMap(_.fuConfigs).map(_.fuType === issueIn.issue.bits.uop.ctrl.fuType).reduce(_ | _)
    xs_assert(Mux(issueIn.issue.valid, issueFuHit, true.B))
  }
}
