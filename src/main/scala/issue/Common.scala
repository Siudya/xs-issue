package issue
import chisel3._
import chisel3.util._
import common._
import fu.fpu.{FMAMidResult, FMAMidResultIO}

abstract class BasicStatusArrayEntry(val srcNum:Int, isIntSrc:Boolean) extends XSBundle{
  val psrc = Vec(srcNum, UInt(if(isIntSrc)GprIdxWidth.W else FprIdxWidth.W))
  val pdest = UInt(if(isIntSrc)GprIdxWidth.W else FprIdxWidth.W)
  val srcType = Vec(srcNum, SrcType())
  val srcState = Vec(srcNum, SrcState())
  val lpv = Vec(srcNum, Vec(loadUnitNum, UInt(LpvLength.W)))
  val fuType = FuType()
  val rfWen = Bool()
  val fpWen = Bool()
  val robIdx = new RobPtr
}
class MemoryStatusArrayEntry extends BasicStatusArrayEntry(2, true)

class SelectInfo extends XSBundle{
  val fuType = FuType()
  val lpv = Vec(loadUnitNum, UInt(LpvLength.W))
  val pdest = UInt(MaxRegfileIdxWidth.W)
  val rfWen = Bool()
  val fpWen = Bool()
  val robPtr = new RobPtr
}

class IntegerSelectInfo extends SelectInfo

class FloatingSelectInfo extends SelectInfo

class BasicWakeupInfo extends XSBundle{
  val pdest = UInt(MaxRegfileIdxWidth.W)
  val robPtr = new RobPtr
}
class WakeUpInfo extends BasicWakeupInfo{
  val lpv = Vec(loadUnitNum, UInt(LpvLength.W))
}
class EarlyWakeUpInfo extends BasicWakeupInfo{
  val lpv = UInt(LpvLength.W)
}

object RsType{
  def int = 0
  def mem = 1
  def fp = 2
}

case class RsParam
(
  name:String,
  rsType:Int,
  entriesNum:Int = 48,
  speckWakeupPort:Int,
  //Unchangeable parameters
  bankNum:Int = 4
){
  val isIntRs = rsType == RsType.int
  val isMemRs = rsType == RsType.mem
  val isFpRs = rsType == RsType.fp
  val isLegal = isIntRs || isMemRs || isFpRs
}

case class DispatchParam
(
  name: String,
  width: Int
)
class IssueBundle extends XSBundle {
  val issue = DecoupledIO(new ExuInput)
  val fmaMidStateIssue = ValidIO(new FMAMidResult)
  val fmaWaitForAdd = Output(Bool())
  val fmaMidStateFeedBack = Flipped(ValidIO(new FMAMidResult))
}