package writeback
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import common.{ExuOutput, Redirect}
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
    })

    for(s <- wbSink){
      val sinkParam = s._2._2
      val source = sinkParam.map(elm => wbSourcesMap(elm))
      val sink = s._1
      sink.zip(source).foreach({case(dst, (src,cfg)) =>
        dst := src
        if(s._2._1.needWriteback && cfg.speculativeWakeup){
          val realValid = src.valid && !src.bits.uop.robIdx.needFlush(io.redirectIn)
          dst.valid := RegNext(realValid, false.B)
          dst.bits.uop := RegEnable(src.bits.uop, realValid)
        }
      })
    }

    val redirectOut = wbSources.filter(_._2.hasRedirectOut).map(elm =>{
      val redirectBundle = elm._1
      val redirectParam = elm._2
      val out = IO(Output(Valid(new ExuOutput)))
      out := redirectBundle
      (out, redirectParam)
    })
  }
}
