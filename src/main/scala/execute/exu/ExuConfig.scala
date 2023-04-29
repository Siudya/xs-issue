package execute.exu
import execute.fu.FuConfig
import chisel3._

object ExuType{
  def jmp = 0
  def alu = 1
  def mul = 2
  def div = 3
  def ldu = 4
  def sta = 5
  def std = 6
  def fmisc = 7
  def fmac = 8
  def fdiv = 9
  def i2f = 10
  def vred = 11
  def vmisc = 12
  def vfp = 13
  def vint = 14

  private val mapping = Map(
    jmp -> "jmp",
    alu -> "alu",
    mul -> "mul",
    div -> "div",
    ldu -> "ldu",
    sta -> "sta",
    std -> "std",
    fmisc -> "fmisc",
    fmac -> "fmac",
    fdiv -> "fdiv",
    i2f -> "i2f",
    vred -> "vred",
    vmisc -> "vmisc",
    vfp -> "vfp",
    vint -> "vint"
  )

  def intTypes: Seq[Int] = Seq(jmp, alu, mul, div, i2f)
  def memTypes: Seq[Int] = Seq(ldu, sta, std)
  def fpTypes: Seq[Int] = Seq(fmisc, fmac, fdiv)
  def vecTypes: Seq[Int] = Seq(vred, vmisc, vfp, vint)
  def typeToString(in:Int):String = mapping(in)
  def bypassIntList: Seq[Int] = Seq(alu, mul, ldu)
  def bypassFpList: Seq[Int] = Seq(ldu)
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
  val intSrcNum:Int = fuConfigs.map(_.numIntSrc).max
  val fpSrcNum:Int = fuConfigs.map(_.numFpSrc).max
  val hasFastWakeup: Boolean = fuConfigs.map(_.latency).max != Int.MaxValue
  val latency: Int = fuConfigs.map(_.latency).max
  val exceptionOut: Seq[Int] = fuConfigs.map(_.exceptionOut).reduce(_ ++ _).distinct.sorted
  val writeIntRf = fuConfigs.map(_.writeIntRf).reduce(_||_)
  val writeFpRf = fuConfigs.map(_.writeFpRf).reduce(_||_)
  val writeVecRf = fuConfigs.map(_.writeVecRf).reduce(_||_)
  val wakeUpIntRs = fuConfigs.map(_.writeIntRf).reduce(_||_) && !hasFastWakeup
  val wakeUpFpRs = fuConfigs.map(_.writeFpRf).reduce(_||_) && !hasFastWakeup
  val wakeUpMemRs =  fuConfigs.map(e => e.writeIntRf || e.writeFpRf).reduce(_||_) && !hasFastWakeup
  val writeFloatFlags = fuConfigs.map(_.writeFflags).reduce(_||_)
  val hasRedirectOut = fuConfigs.map(_.hasRedirect).reduce(_||_)
  val isIntType = ExuType.intTypes.contains(exuType)
  val isMemType = ExuType.memTypes.contains(exuType)
  val isFpType = ExuType.fpTypes.contains(exuType)
  val isVecType = ExuType.vecTypes.contains(exuType)
  val bypassIntRegfile = ExuType.bypassIntList.contains(exuType)
  val bypassFpRegfile = ExuType.bypassFpList.contains(exuType)

  override def toString = s"\n\t${name}: intSrcNum: ${intSrcNum} fpSrcNum: ${fpSrcNum} Type: ${ExuType.typeToString(exuType)} " +
    "\n\t\t Functions Units: " + fuConfigs.map(_.toString + " ").reduce(_++_)
}
