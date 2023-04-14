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
class FloatStatusArrayEntry extends BasicStatusArrayEntry(3, false)

class SelectInfo extends XSBundle{
  val fuType = FuType()
  val lpv = Vec(loadUnitNum, UInt(LpvLength.W))
  val pdest = UInt(MaxRegfileIdxWidth.W)
  val rfWen = Bool()
  val fpWen = Bool()
  val robPtr = new RobPtr
}

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
class RsIssueBundle(fuSelWidth:Int) extends XSBundle{
  val valid = Output(Bool())
  val uop = Output(new MicroOp)
  val src = Output(Vec(3, UInt(XLEN.W)))
  val fuSel = Output(UInt(fuSelWidth.W))
  val fmaMidState = ValidIO(new FMAMidResult)
  val fmaWaitForAdd = Output(Bool())
}
class RsFeedbackBundle(fuSelWidth:Int) extends XSBundle{
  val release = Input(UInt(fuSelWidth.W))
  val fmaMidState = Flipped(ValidIO(new FMAMidResult))
  val ready = Input(Bool())
}

class IssueBundle(fuSelWidth:Int) extends XSBundle {
  val issue = Output(new RsIssueBundle(fuSelWidth))
  val feedback = Input(new RsFeedbackBundle(fuSelWidth))
  def fire:Bool = issue.valid & feedback.ready
}