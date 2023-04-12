package fu
import chisel3._
import common.{FuType, MicroOp}
case class FuConfig
(
  name: String,
  fuType: UInt,
  numIntSrc: Int,
  numFpSrc: Int,
  writeIntRf: Boolean,
  writeFpRf: Boolean,
  writeFflags: Boolean,
  latency: Int,
  hasRedirect: Boolean,
  exceptionOut: Seq[Int] = Seq(),
) {
  def srcCnt: Int = math.max(numIntSrc, numFpSrc)
  override def toString = name
}

object FuConfigs{
  val aluCfg = FuConfig(
    name = "alu",
    fuType = FuType.alu,
    numIntSrc = 2,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    writeFflags = false,
    latency = 0,
    hasRedirect = true
  )
  val mulCfg = FuConfig(
    name = "mul",
    fuType = FuType.mul,
    numIntSrc = 2,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    writeFflags = false,
    latency = 1, //Actual latency is 2. It is reduced to 1 because of bypass network
    hasRedirect = false
  )
  val bkuCfg = FuConfig(
    name = "bku",
    fuType = FuType.bku,
    numIntSrc = 2,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    writeFflags = false,
    latency = 1, //Actual latency is 2. It is reduced to 1 because of bypass network
    hasRedirect = false
  )
  val divCfg = FuConfig(
    name = "div",
    FuType.div,
    numIntSrc = 2,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    writeFflags = false,
    latency = Int.MaxValue,
    hasRedirect = false
  )
}