package issue
import chisel3._
import chisel3.util._
import common._
import execute.fu.fpu.{FMAMidResult, FMAMidResultIO}

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

class BasicWakeupInfo extends XSBundle{
  val pdest = UInt(MaxRegfileIdxWidth.W)
  val destType = SrcType()
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
  def vec = 3
}

case class RsParam
(
  name:String,
  rsType:Int,
  entriesNum:Int = 48,
  speckWakeupPort:Int,
  //Unchangeable parameters
  bankNum:Int = 4,
  passThroughIntRf:Boolean = false,
  passThroughFpRf:Boolean = false,
  passThroughVecRf:Boolean = false
){
  val isIntRs = rsType == RsType.int
  val isMemRs = rsType == RsType.mem
  val isFpRs = rsType == RsType.fp
  val isVecRs = rsType == RsType.vec
  val isLegal = isIntRs || isMemRs || isFpRs || isVecRs
  def TypeName:String = {
    require(isLegal)
    if(isIntRs){
      "Integer RS "
    } else if(isFpRs) {
      "Floating RS "
    } else if(isVecRs) {
      "Vector RS "
    } else {
      "Memory RS"
    }
  }
}

case class DispatchParam
(
  name: String,
  width: Int
)
class IssueBundle(bankNum:Int, entryNum:Int) extends XSBundle {
  val issue = DecoupledIO(new ExuInput)
  val rsIdx = Output(new RsIdx(bankNum, entryNum))
  val fmaMidState = Flipped(new FMAMidResultIO)
  val fuInFire = Input(Bool())
  val rsFeedback = Flipped(new RSFeedbackIO)
  val stIssuePtr = Input(new SqPtr())
}

class RsIdx(bankNum:Int, entryNum:Int) extends Bundle{
  val bankIdxOH = UInt(bankNum.W)
  val entryIdxOH = UInt(entryNum.W)
}