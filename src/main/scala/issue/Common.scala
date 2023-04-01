package issue
import chisel3._
import chisel3.util._
import xs.utils.{CircularQueuePtr, HasCircularQueuePtrHelper, SignExt, ZeroExt}

class FtqPtr extends CircularQueuePtr[FtqPtr](64)
class LqPtr extends CircularQueuePtr[LqPtr](80)
class SqPtr extends CircularQueuePtr[SqPtr](64)
class RobPtr extends CircularQueuePtr[RobPtr](160) with HasCircularQueuePtrHelper

object SrcState {
  def busy = "b0".U
  def rdy = "b1".U
  // def specRdy = "b10".U // speculative ready, for future use
  def apply() = UInt(1.W)
}
object FenceOpType {
  def fence = "b10000".U
  def sfence = "b10001".U
  def fencei = "b10010".U
  def nofence = "b00000".U
}
object FuType {
  def jmp = "b0000".U
  def i2f = "b0001".U
  def csr = "b0010".U
  def alu = "b0110".U
  def mul = "b0100".U
  def div = "b0101".U
  def fence = "b0011".U
  def bku = "b0111".U
  def fmac = "b1000".U
  def fmisc = "b1011".U
  def fDivSqrt = "b1010".U
  def ldu = "b1100".U
  def stu = "b1101".U
  def mou = "b1111".U // for amo, lr, sc, fence
  def X = BitPat("b????")
  def num = 14
  def apply() = UInt(log2Up(num).W)
  def isIntExu(fuType: UInt) = !fuType(3)
  def isJumpExu(fuType: UInt) = fuType === jmp
  def isFpExu(fuType: UInt) = fuType(3, 2) === "b10".U
  def isMemExu(fuType: UInt) = fuType(3, 2) === "b11".U
  def isLoadStore(fuType: UInt) = isMemExu(fuType) && !fuType(1)
  def isStoreExu(fuType: UInt) = isMemExu(fuType) && fuType(0)
  def isAMO(fuType: UInt) = fuType(1)
  def isFence(fuType: UInt) = fuType === fence
  def isSvinvalBegin(fuType: UInt, func: UInt, flush: Bool) = isFence(fuType) && func === FenceOpType.nofence && !flush
  def isSvinval(fuType: UInt, func: UInt, flush: Bool) = isFence(fuType) && func === FenceOpType.sfence && !flush
  def isSvinvalEnd(fuType: UInt, func: UInt, flush: Bool) = isFence(fuType) && func === FenceOpType.nofence && flush
  def jmpCanAccept(fuType: UInt) = !fuType(2)
  def mduCanAccept(fuType: UInt) = fuType(2) && !fuType(1) || fuType(2) && fuType(1) && fuType(0)
  def aluCanAccept(fuType: UInt) = fuType(2) && fuType(1) && !fuType(0)
  def fmacCanAccept(fuType: UInt) = !fuType(1)
  def fmiscCanAccept(fuType: UInt) = fuType(1)
  def loadCanAccept(fuType: UInt) = !fuType(0)
  def storeCanAccept(fuType: UInt) = fuType(0)
  def storeIsAMO(fuType: UInt) = fuType(1)
  val functionNameMap = Map(
    jmp.litValue -> "jmp",
    i2f.litValue -> "int_to_float",
    csr.litValue -> "csr",
    alu.litValue -> "alu",
    mul.litValue -> "mul",
    div.litValue -> "div",
    fence.litValue -> "fence",
    bku.litValue -> "bku",
    fmac.litValue -> "fmac",
    fmisc.litValue -> "fmisc",
    fDivSqrt.litValue -> "fdiv/fsqrt",
    ldu.litValue -> "load",
    stu.litValue -> "store",
    mou.litValue -> "mou"
  )
}
object FuOpType {
  def apply() = UInt(7.W)
  def X = BitPat("b???????")
}

object SelImm {
  def IMM_X = "b0111".U
  def IMM_S = "b0000".U
  def IMM_SB = "b0001".U
  def IMM_U = "b0010".U
  def IMM_UJ = "b0011".U
  def IMM_I = "b0100".U
  def IMM_Z = "b0101".U
  def INVALID_INSTR = "b0110".U
  def IMM_B6 = "b1000".U
  def X = BitPat("b????")
  def apply() = UInt(4.W)
}
class XSBundle extends Bundle{
  val GprEntriesNum = 128
  val FprEntriesNum = 128
  def GprIdxWidth = log2Ceil(GprEntriesNum)
  def FprIdxWidth = log2Ceil(FprEntriesNum)
  val LpvLength = 4
}
abstract class Imm(val len: Int) extends Bundle {
  def toImm32(minBits: UInt): UInt = do_toImm32(minBits(len - 1, 0))
  def do_toImm32(minBits: UInt): UInt
  def minBitsFromInstr(instr: UInt): UInt
}
case class Imm_I() extends Imm(12) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits(len - 1, 0), 32)
  override def minBitsFromInstr(instr: UInt): UInt =
    Cat(instr(31, 20))
}
case class Imm_S() extends Imm(12) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)
  override def minBitsFromInstr(instr: UInt): UInt =
    Cat(instr(31, 25), instr(11, 7))
}
case class Imm_B() extends Imm(12) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(Cat(minBits, 0.U(1.W)), 32)
  override def minBitsFromInstr(instr: UInt): UInt =
    Cat(instr(31), instr(7), instr(30, 25), instr(11, 8))
}
case class Imm_U() extends Imm(20){
  override def do_toImm32(minBits: UInt): UInt = Cat(minBits(len - 1, 0), 0.U(12.W))
  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(31, 12)
  }
}
case class Imm_J() extends Imm(20){
  override def do_toImm32(minBits: UInt): UInt = SignExt(Cat(minBits, 0.U(1.W)), 32)
  override def minBitsFromInstr(instr: UInt): UInt = {
    Cat(instr(31), instr(19, 12), instr(20), instr(30, 25), instr(24, 21))
  }
}
case class Imm_Z() extends Imm(12 + 5){
  override def do_toImm32(minBits: UInt): UInt = minBits
  override def minBitsFromInstr(instr: UInt): UInt = {
    Cat(instr(19, 15), instr(31, 20))
  }
}
case class Imm_B6() extends Imm(6){
  override def do_toImm32(minBits: UInt): UInt = ZeroExt(minBits, 32)
  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(25, 20)
  }
}
object ImmUnion {
  val I = Imm_I()
  val S = Imm_S()
  val B = Imm_B()
  val U = Imm_U()
  val J = Imm_J()
  val Z = Imm_Z()
  val B6 = Imm_B6()
  val imms = Seq(I, S, B, U, J, Z, B6)
  val maxLen = imms.maxBy(_.len).len
  val immSelMap = Seq(
    SelImm.IMM_I,
    SelImm.IMM_S,
    SelImm.IMM_SB,
    SelImm.IMM_U,
    SelImm.IMM_UJ,
    SelImm.IMM_Z,
    SelImm.IMM_B6
  ).zip(imms)
  println(s"ImmUnion max len: $maxLen")
}

object BrType {
  def notCFI   = "b00".U
  def branch  = "b01".U
  def jal     = "b10".U
  def jalr    = "b11".U
  def apply() = UInt(2.W)
}

object SrcType {
  def reg = "b00".U
  def pc = "b01".U
  def imm = "b01".U
  def fp = "b10".U
  def DC = imm // Don't Care
  def X = BitPat("b??")
  def isReg(srcType: UInt) = srcType === reg
  def isPc(srcType: UInt) = srcType === pc
  def isImm(srcType: UInt) = srcType === imm
  def isFp(srcType: UInt) = srcType(1)
  def isPcOrImm(srcType: UInt) = srcType(0)
  def isRegOrFp(srcType: UInt) = !srcType(0)
  def regIsFp(srcType: UInt) = srcType(1)
  def apply() = UInt(2.W)
}

class PreDecodeInfo extends Bundle {  // 8 bit
  val valid   = Bool()
  val isRVC   = Bool()
  val brType  = UInt(2.W)
  val isCall  = Bool()
  val isRet   = Bool()
}

class FPUCtrlSignals extends Bundle {
  val isAddSub = Bool() // swap23
  val typeTagIn = UInt(1.W)
  val typeTagOut = UInt(1.W)
  val fromInt = Bool()
  val wflags = Bool()
  val fpWen = Bool()
  val fmaCmd = UInt(2.W)
  val div = Bool()
  val sqrt = Bool()
  val fcvt = Bool()
  val typ = UInt(2.W)
  val fmt = UInt(2.W)
  val ren3 = Bool() //TODO: remove SrcType.fp
  val rm = UInt(3.W)
}

class CfCtrl extends Bundle {
  val cf = new CtrlFlow
  val ctrl = new CtrlSignals
}

class CtrlFlow extends Bundle {
  val pd = new PreDecodeInfo
  val pred_taken = Bool()
  val storeSetHit = Bool() // inst has been allocated an store set
  val ssid = UInt(5.W)
  val ftqPtr = new FtqPtr
  val ftqOffset = UInt(log2Up(16).W)
}

class CtrlSignals extends Bundle {
  val srcType = Vec(3, SrcType())
  val fuType = FuType()
  val fuOpType = FuOpType()
  val rfWen = Bool()
  val fpWen = Bool()
  val selImm = SelImm()
  val imm = UInt(ImmUnion.maxLen.W)
  val fpu = new FPUCtrlSignals
}

//class MicroOp extends CfCtrl {
//  val srcState = Vec(3, SrcState())
//  val psrc = Vec(3, UInt(7.W))
//  val pdest = UInt(7.W)
//  val old_pdest = UInt(7.W)
//  val robIdx = new RobPtr
//  val lqIdx = new LqPtr
//  val sqIdx = new SqPtr
//  val eliminatedMove = Bool()
//}

class MicroOp extends Bundle {
  val payload = UInt(128.W)
  val robIdx = new RobPtr
}

abstract class BasicStatusArrayEntry(srcNum:Int, isIntSrc:Boolean, withLPV:Boolean) extends XSBundle{
  val psrc = Vec(srcNum, UInt(if(isIntSrc)GprIdxWidth.W else FprIdxWidth.W))
  val srcState = Vec(srcNum, UInt(if(withLPV)LpvLength.W else 1.W))
  val fuType = FuType()
  val robIdx = new RobPtr
  def toIssueInfo:Valid[SelectInfo]
}
class MemoryStatusArrayEntry extends BasicStatusArrayEntry(2, true,true){
  def toIssueInfo: Valid[SelectInfo] = {
    val src0Ready = srcState(0) === Fill(LpvLength, 1.U)
    val src1Ready = srcState(1) === Fill(LpvLength, 1.U)
    val src0ReadyLpv = PopCount(srcState(1)) === 1.U
    val src1ReadyLpv = PopCount(srcState(1)) === 1.U
    val checkSrcState = (src0Ready & src1Ready) | (src0Ready & src1ReadyLpv) | (src0ReadyLpv & src1Ready)
    val res = Wire(Valid(new SelectInfo))
    res.valid := checkSrcState
    res.bits.fuType := fuType
    res.bits.robPtr := robIdx
    res
  }
}
class FloatStatusArrayEntry extends BasicStatusArrayEntry(3, false,true){
  def toIssueInfo: Valid[SelectInfo] = {
    val src0Ready = srcState(0) === Fill(LpvLength, 1.U)
    val src1Ready = srcState(1) === Fill(LpvLength, 1.U)
    val src0ReadyLpv = PopCount(srcState(1)) === 1.U
    val src1ReadyLpv = PopCount(srcState(1)) === 1.U
    val checkSrcState = (src0Ready & src1Ready) | (src0Ready & src1ReadyLpv) | (src0ReadyLpv & src1Ready)
    val res = Wire(Valid(new SelectInfo))
    res.valid := checkSrcState
    res.bits.fuType := fuType
    res.bits.robPtr := robIdx
    res
  }
}

class SelectInfo extends Bundle{
  val fuType = FuType()
  val robPtr = new RobPtr
}