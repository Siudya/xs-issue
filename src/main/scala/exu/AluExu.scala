package exu
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import common.Redirect
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, LazyModuleImpLike}

class AluExu(cfg:ExuConfig)(implicit p:Parameters) extends LazyModule{
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutNode(cfg)

  override def module: LazyModuleImpLike = new AluExuImpl(this)
}
class AluExuImpl(outer:AluExu)(implicit p:Parameters) extends LazyModuleImp(outer){
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
  })

}
