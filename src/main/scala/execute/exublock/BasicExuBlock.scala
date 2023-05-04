package execute.exublock

import chipsalliance.rocketchip.config.Parameters
import chisel3.util.Valid
import chisel3.{Input, Vec}
import common.{ExuOutput, Redirect}
import execute.exucx.BasicExuComplex
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

abstract class BasicExuBlock(implicit p:Parameters) extends LazyModule{
  val issueNode = new ExuBlockIssueNode
  val writebackNode = new ExuBlockWritebackNode
  override def module:BasicExuBlockImp
}

class BasicExuBlockImp(outer:BasicExuBlock) extends LazyModuleImp(outer){
  val redirectIn = IO(Input(Valid(new Redirect)))
  outer.writebackNode.in.zip(outer.writebackNode.out).foreach({
    case (source, sink) =>
      sink._1 := source._1
  })
  outer.issueNode.in.zip(outer.issueNode.out).foreach({
    case (source, sink) =>
      sink._1 <> source._1
  })
}