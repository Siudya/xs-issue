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

  def intType: Seq[Int] = Seq(jmp, alu, mul, div)
  def memType: Seq[Int] = Seq(load, sta, std)
  def fpType: Seq[Int] = Seq(fmisc, fmac)
  def maybeBlockType:Seq[Int] = Seq(div, fmac, fmisc)
  def typeToString(in:Int):String = mapping(in)
}

case class ExuConfig
(
  name: String,
  id: Int,
  blockName: String, // NOTE: for perf counter
  fuConfigs: Seq[FuConfig],
  exuType:Int,
  srcNum:Int,
  releaseWidth:Int = 0
){
  val hasFastWakeup: Boolean = fuConfigs.map(_.latency).max != Int.MaxValue
  val latency: Int = fuConfigs.map(_.latency).max
  val exceptionOut: Seq[Int] = fuConfigs.map(_.exceptionOut).reduce(_ ++ _).distinct.sorted

  override def toString = s"${name} #${id} belongs to ${blockName}: srcNum: ${srcNum} Type: ${ExuType.typeToString(exuType)} " +
    s"\n\tFunction Units: " + fuConfigs.toString()
}
