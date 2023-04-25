package execute.exucx
import chisel3._
import chisel3.util.Valid
import chipsalliance.rocketchip.config.Parameters
import common.Redirect
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

abstract class BasicExuComplex(implicit p:Parameters) extends LazyModule{
  val issueNode = new ExuComplexIssueNode
  val writebackNode = new ExuComplexWritebackNode

  override def module:BasicExuComplexImp
}

abstract class BasicExuComplexImp(outer:BasicExuComplex) extends LazyModuleImp(outer){
  val redirectIn = IO(Input(Valid(new Redirect)))
  outer.writebackNode.in.zip(outer.writebackNode.out).foreach({
    case (source, sink) =>
      sink._1 := source._1
  })
}