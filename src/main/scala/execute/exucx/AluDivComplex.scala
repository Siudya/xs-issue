package execute.exucx
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.Valid
import common.{ExuOutput, Redirect}
import exu.{AluExu, DivExu}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

class AluJmpComplex(id: Int, bypassNum:Int)(implicit p:Parameters) extends LazyModule{
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
    private val issueRouted = issueNode.out.map(_._1)
    issueRouted.foreach(_ <> issueIn)

    alu.module.io.bypassIn := io.bypassIn
    alu.module.redirectIn := io.redirect

    div.module.io.bypassIn := io.bypassIn
    div.module.redirectIn := io.redirect
  }
}
