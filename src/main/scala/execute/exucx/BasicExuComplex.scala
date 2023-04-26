package execute.exucx
import chisel3._
import chisel3.util.Valid
import chipsalliance.rocketchip.config.Parameters
import common.{ExuOutput, Redirect}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

abstract class BasicExuComplex(implicit p:Parameters) extends LazyModule{
  val issueNode = new ExuComplexIssueNode
  val writebackNode = new ExuComplexWritebackNode

  override def module:BasicExuComplexImp
}

abstract class BasicExuComplexImp(outer:BasicExuComplex, bypassNum:Int) extends LazyModuleImp(outer){
  val redirectIn = IO(Input(Valid(new Redirect)))
  val bypassIn = IO(Input(Vec(bypassNum, Valid(new ExuOutput))))
  outer.writebackNode.in.zip(outer.writebackNode.out).foreach({
    case (source, sink) =>
      sink._1 := source._1
  })
}