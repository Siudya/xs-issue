package execute.exu
import execute.fu.FuConfig
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
  def fdiv = 9
  def i2f = 10

  private val mapping = Map(
    jmp -> "jmp",
    alu -> "alu",
    mul -> "mul",
    div -> "div",
    load -> "load",
    sta -> "sta",
    std -> "std",
    fmisc -> "fmisc",
    fmac -> "fmac",
    fdiv -> "fdiv",
    i2f -> "i2f"
  )

  def intTypes: Seq[Int] = Seq(jmp, alu, mul, div, i2f)
  def memTypes: Seq[Int] = Seq(load, sta, std)
  def fpTypes: Seq[Int] = Seq(fmisc, fmac, fdiv)
  def typeToString(in:Int):String = mapping(in)
}

case class ExuConfig
(
  name: String,
  id:Int,
  complexName: String,
  fuConfigs: Seq[FuConfig],
  exuType:Int,
  needToken:Boolean = false,
  speculativeWakeup:Boolean = false
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

  override def toString = s"\n\t${name}: srcNum: ${srcNum} Type: ${ExuType.typeToString(exuType)} " +
    "\n\t\t Functions Units: " + fuConfigs.map(_.toString + " ").reduce(_++_)
}
