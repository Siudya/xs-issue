package exu
import fu.FuConfig
import chisel3._

object ExuType{
  def jmp = 0
  def alu = 1
  def mul = 2
  def div = 3
  def load = 4
  def sta = 5
  def std = 6
  def fmisc = 7
  def fmac = 8

  private val mapping = Map(
    jmp -> "jmp",
    alu -> "alu",
    mul -> "mul",
    div -> "div",
    load -> "load",
    sta -> "sta",
    std -> "std",
    fmisc -> "fmisc",
    fmac -> "fmac"
  )

  def intTypes: Seq[Int] = Seq(jmp, alu, mul, div)
  def memTypes: Seq[Int] = Seq(load, sta, std)
  def fpTypes: Seq[Int] = Seq(fmisc, fmac)
  def typeToString(in:Int):String = mapping(in)
}

case class ExuConfig
(
  name: String,
  id: Int,
  blockName: String, // NOTE: for perf counter
  fuConfigs: Seq[FuConfig],
  exuType:Int,
  releaseWidth:Int = 0
){
  val srcNum:Int = fuConfigs.map(_.srcCnt).max
  val hasFastWakeup: Boolean = fuConfigs.map(_.latency).max != Int.MaxValue
  val latency: Int = fuConfigs.map(_.latency).max
  val exceptionOut: Seq[Int] = fuConfigs.map(_.exceptionOut).reduce(_ ++ _).distinct.sorted
  val writeIntRf = fuConfigs.map(_.writeIntRf).reduce(_||_)
  val writeFpRf = fuConfigs.map(_.writeFpRf).reduce(_||_)
  val wakeUpIntRs = fuConfigs.map(_.writeIntRf).reduce(_||_) && !hasFastWakeup
  val wakeUpFpRs = fuConfigs.map(_.writeFpRf).reduce(_||_) && !hasFastWakeup
  val wakeUpMemRs =  fuConfigs.map(e => e.writeIntRf || e.writeFpRf).reduce(_||_) && !hasFastWakeup
  val writeFloatFlags = fuConfigs.map(_.writeFflags).reduce(_||_)
  val hasRedirectOut = fuConfigs.map(_.hasRedirect).reduce(_||_)
  val isIntType = ExuType.intTypes.contains(exuType)
  val isMemType = ExuType.memTypes.contains(exuType)
  val isFpType = ExuType.fpTypes.contains(exuType)

  override def toString = s"${name} #${id} belongs to ${blockName}: srcNum: ${srcNum} Type: ${ExuType.typeToString(exuType)} " +
    s"\n\tFunction Units: " + fuConfigs.toString()
}
