package fu
import chisel3._
import common.{FuType, MicroOp}
case class FuConfig
(
  name: String,
  fuSel: MicroOp => Bool,
  fuType: UInt,
  numIntSrc: Int,
  numFpSrc: Int,
  writeIntRf: Boolean,
  writeFpRf: Boolean,
  writeFflags: Boolean = false,
  latency: Int,
  hasRedirect: Boolean = false,
  exceptionOut: Seq[Int] = Seq(),
) {
  def srcCnt: Int = math.max(numIntSrc, numFpSrc)
}

object FuConfigs{
  val aluCfg = FuConfig(
    name = "alu",
    fuSel = (uop: MicroOp) => uop.ctrl.fuType === FuType.alu,
    fuType = FuType.alu,
    numIntSrc = 2,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    latency = 1,
    hasRedirect = true
  )
}