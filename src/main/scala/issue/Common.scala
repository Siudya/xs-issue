package issue
import chisel3._
import chisel3.util._
import common._

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

case class RsParam
(
  entriesNum:Int = 48,
  //Unchangeable parameters
  bankNum:Int = 4
)

case class DispatchParam
(
  name: String,
  width: Int
)
class IssueBundle(rlsWidth:Int) extends XSBundle{
  val uop = Valid(new MicroOp)
  val release = Input(UInt(rlsWidth.W))
}