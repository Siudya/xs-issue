package regfile
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.Parameters
import writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}

class RegFileTop(implicit p:Parameters) extends LazyModule{
  val issueInNode = new RegFileTopInNode
  val writebackNode = new WriteBackSinkNode(WriteBackSinkParam("RegFile Top", WriteBackSinkType.regFile))
  lazy val module = new LazyModuleImp(this){

  }
}
