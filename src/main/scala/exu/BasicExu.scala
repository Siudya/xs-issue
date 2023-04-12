package exu
import chisel3.util._
import chisel3._
import chipsalliance.rocketchip.config.Parameters
import common.Redirect
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

abstract class BasicExu(implicit p:Parameters) extends LazyModule{
  def issueNode: ExuInputNode
  def writebackNode: ExuOutNode
  def module:BasicExuImpl
}

abstract class BasicExuImpl(outer:BasicExu) extends LazyModuleImp(outer){
  val redirectIn = IO(Input(Valid(new Redirect)))
}
