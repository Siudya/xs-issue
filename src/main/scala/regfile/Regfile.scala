package regfile
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.config.Parameters
class Regfile(implicit p: Parameters) extends LazyModule{
  override def module = new RegfileImpl(this)
}

class RegfileImpl(outer: Regfile) extends LazyModuleImp(outer){

}