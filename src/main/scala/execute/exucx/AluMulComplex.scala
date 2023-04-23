package execute.exucx

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.Valid
import common.{ExuOutput, Redirect}
import exu.{AluExu, MulExu}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

class AluMulComplex(id: Int, bypassNum:Int)(implicit p:Parameters) extends LazyModule{
  val issueNode = new ExuComplexIssueNode
  val writebackNode = new ExuComplexWritebackNode
  val alu = new AluExu(id, bypassNum + 1)
  val mul = new MulExu(id, bypassNum)
  alu.issueNode :*= issueNode
  mul.issueNode :*= issueNode
  writebackNode :=* alu.writebackNode
  writebackNode :=* mul.writebackNode
  lazy val module = new LazyModuleImp(this){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 2)
    val io = IO(new Bundle{
      val redirect = Input(Valid(new Redirect))
      val bypassIn = Input(Vec(bypassNum, Valid(new ExuOutput)))
      val bypassOut = Output(Valid(new ExuOutput))
    })
    private val issueIn = issueNode.in.head._1
    private val issueRouted = issueNode.out.map(_._1)
    issueRouted.foreach(_ <> issueIn)

    alu.module.io.bypassIn.take(bypassNum).zip(io.bypassIn).foreach({case(a, b) => a := b})
    alu.module.redirectIn := io.redirect

    mul.module.io.bypassIn := io.bypassIn
    mul.module.redirectIn := io.redirect

    alu.module.io.bypassIn.last := mul.module.io.bypassOut
    io.bypassOut := mul.module.io.bypassOut
  }
}
