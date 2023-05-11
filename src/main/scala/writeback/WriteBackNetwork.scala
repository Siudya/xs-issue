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

    println("\nWriteback Network Info:")
    println(s"Writeback Num: ${wbSources.length}")
    val io = IO(new Bundle{
      val redirectIn = Input(Valid(new Redirect))
      val redirectOut = Output(Vec(wbSources.count(_._2.hasRedirectOut), Valid(new ExuOutput)))
    })
    val redirectOutParam = wbSources.filter(_._2.hasRedirectOut).zip(io.redirectOut).map({case(source, sink) =>
      sink.valid := source._1.valid
      sink.bits := source._1.bits
      source._2
    })

    for(s <- wbSink){
      val sinkParam = s._2._2
      val source = sinkParam.map(elm => wbSourcesMap(elm))
      val sink = s._1
      sink.zip(source).foreach({case(dst, (src,cfg)) =>
        val realSrc = WireInit(src)
        if(s._2._1.needWriteback && cfg.speculativeWakeup){
          val realValid = src.valid && !src.bits.uop.robIdx.needFlush(io.redirectIn)
          realSrc.valid := RegNext(realValid, false.B)
          realSrc.bits.uop := RegEnable(src.bits.uop, realValid)
        }
        if(s._2._1.isRob){
          dst.valid := RegNext(realSrc.valid, false.B)
          dst.bits := RegEnable(realSrc.bits, realSrc.valid)
        } else {
          dst := realSrc
        }
      })
    }
  }
}
