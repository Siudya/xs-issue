package writeback
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import common.Redirect
import freechips.rocketchip.diplomacy._
class WriteBackNetwork(implicit p:Parameters) extends LazyModule{
  val node = new WriteBackNetworkNode

  lazy val module = new LazyModuleImp(this){
    private val wbSources = node.in
    private val wbSourcesMap = node.in.map(elm => elm._2 -> (elm._1, elm._2)).toMap
    private val wbSink = node.out
    private val fflagsNum = wbSources.count(_._2.writeFloatFlags)
    private val redirectOutNum = wbSources.count(_._2.hasRedirectOut)
    val io = IO(new Bundle{
      val redirectIn = Input(Valid(new Redirect))
      val fflagsWrite = Output(Vec(fflagsNum, Valid(UInt(5.W))))
      val redirectOut = Output(Vec(redirectOutNum, Valid(new Redirect)))
    })

    for(s <- wbSink){
      val sinkParam = s._2._2
      val source = sinkParam.map(elm => wbSourcesMap(elm))
      val sink = s._1
      sink.zip(source).foreach({case(dst, (src,cfg)) =>
        dst := src
        if(s._2._1.needWriteback && cfg.speculativeWakeup){
          dst.valid := RegNext(src.valid, false.B)
          dst.bits.uop := RegEnable(src.bits.uop, src.valid)
        }
      })
    }

    for((f, fromExu) <- io.fflagsWrite.zip(wbSources.filter(_._2.writeFloatFlags).map(_._1))){
      f.valid := fromExu.valid
      f.bits := fromExu.bits.fflags
    }
    for ((r, fromExu) <- io.redirectOut.zip(wbSources.filter(_._2.hasRedirectOut).map(_._1))) {
      r.valid := fromExu.valid & fromExu.bits.redirectValid
      r.bits := fromExu.bits.redirect
    }
  }
}