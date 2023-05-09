package common

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import execute.exu.ExuConfig
import execute.fu.{FuConfig, FuInput}
import issue.RsIdx
import xs.utils.{CircularQueuePtr, HasCircularQueuePtrHelper, SignExt, ZeroExt}
import xs.utils._

class FtqPtr extends CircularQueuePtr[FtqPtr](64)
class LqPtr extends CircularQueuePtr[LqPtr](80)
class SqPtr extends CircularQueuePtr[SqPtr](64)
class RobPtr extends CircularQueuePtr[RobPtr](160) with HasCircularQueuePtrHelper{
  def needFlush(redirect: Valid[Redirect]): Bool = {
    redirect.valid && redirect.bits.shouldBeFlushed(this)
  }
  def needFlush(redirect: Seq[Valid[Redirect]]): Bool = VecInit(redirect.map(needFlush)).asUInt.orR
}

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
  def reg = "b000".U
  def pc = "b001".U
  def imm = "b001".U
  def fp = "b010".U
  def vec: UInt = "b011".U
  def default: UInt = "b100".U
  def DC = imm // Don't Care
  def X = BitPat("b??")
  def isReg(srcType: UInt) = srcType === reg
  def isPc(srcType: UInt) = srcType === pc
  def isImm(srcType: UInt) = srcType === imm
  def isFp(srcType: UInt) = srcType === fp
  def isVec(srcType: UInt) = srcType === vec
  def isPcOrImm(srcType: UInt) = srcType === imm
  def isRegOrFp(srcType: UInt):Bool = srcType === reg || srcType === fp
  def regIsFp(srcType: UInt) = srcType === fp
  def apply() = UInt(3.W)
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

class CfCtrl extends XSBundle {
  val cf = new CtrlFlow
  val ctrl = new CtrlSignals
}

class CtrlFlow extends XSBundle {
  val pc = UInt(VAddrBits.W)
  val pd = new PreDecodeInfo
  val pred_taken = Bool()
  val exceptionVec = ExceptionVec()
  val loadWaitBit = Bool()
  val waitForRobIdx = new RobPtr // store set predicted previous store robIdx
  val storeSetHit = Bool() // inst has been allocated an store set
  val ssid = UInt(5.W)
  val ftqPtr = new FtqPtr
  val ftqOffset = UInt(log2Up(16).W)
}

class CtrlSignals extends Bundle {
  val srcType = Vec(3, SrcType())
  val lsrc = Vec(3, UInt(5.W))
  val ldest = UInt(5.W)
  val fuType = FuType()
  val fuOpType = FuOpType()
  val rfWen = Bool()
  val fpWen = Bool()
  val selImm = SelImm()
  val imm = UInt(ImmUnion.maxLen.W)
  val fpu = new FPUCtrlSignals
}

object RedirectLevel {
  def flushAfter = "b0".U
  def flush = "b1".U
  def apply() = UInt(1.W)
  // def isUnconditional(level: UInt) = level(1)
  def flushItself(level: UInt) = level(0)
  // def isException(level: UInt) = level(1) && level(0)
}
class CfiUpdateInfo extends XSBundle{
  val predTaken = Bool()
  val taken = Bool()
  val isMisPred = Bool()
  val target = UInt(VAddrBits.W)
}
class Redirect extends XSBundle {
  val robIdx = new RobPtr
  val ftqIdx = new FtqPtr
  val ftqOffset = UInt(log2Up(32).W)
  val level = RedirectLevel()
  val interrupt = Bool()
  val cfiUpdate = new CfiUpdateInfo

  val stFtqIdx = new FtqPtr // for load violation predict
  val stFtqOffset = UInt(log2Up(32).W)

  // def isUnconditional() = RedirectLevel.isUnconditional(level)
  def flushItself() = RedirectLevel.flushItself(level)
  // def isException() = RedirectLevel.isException(level)
  def shouldBeFlushed(in:RobPtr) = Mux(flushItself(), in <= robIdx, in < robIdx)
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

class MicroOp extends CfCtrl {
  val srcState = Vec(3, SrcState())
  val psrc = Vec(3, UInt(MaxRegfileIdxWidth.W))
  val pdest = UInt(MaxRegfileIdxWidth.W)
  val old_pdest = UInt(MaxRegfileIdxWidth.W)
  val robIdx = new RobPtr
  val lqIdx = new LqPtr
  val sqIdx = new SqPtr
  val lpv = Vec(loadUnitNum, UInt(LpvLength.W))
}

class ExuInput extends XSBundle {
  val uop = new MicroOp
  val src = Vec(3, UInt(XLEN.W))
}

class ExuOutput extends XSBundle {
  val uop = new MicroOp
  val data = UInt(XLEN.W)
  val fflags = UInt(5.W)
  val redirectValid = Bool()
  val redirect = new Redirect
}

trait XSParam{
  val GprEntriesNum = 128
  val FprEntriesNum = 128
  def GprIdxWidth = log2Ceil(GprEntriesNum)
  def FprIdxWidth = log2Ceil(FprEntriesNum)
  def MaxRegfileIdxWidth = if (GprIdxWidth > FprIdxWidth) GprIdxWidth else FprIdxWidth
  val LpvLength = 5
  val loadUnitNum = 2
  val XLEN = 64
  val VAddrBits = 39
  val AsidLength = 16
  val FtqSize = 64
  val maxEntryInOneRsBank = 16

  val loadQueueSize = 80
  val storeQueueSize = 64
  val IssQueSize = 16
  val CommitWidth = 6
  val TriggerNum = 10
  val PAddrBits = 36
  val PredictWidth = 16
  val instOffsetBits = 1
}
class XSBundle extends Bundle with XSParam
class XSModule extends Module with XSParam

object ExceptionVec {
  def apply() = Vec(16, Bool())
}

object ExceptionNO {
  def instrAddrMisaligned = 0
  def instrAccessFault = 1
  def illegalInstr = 2
  def breakPoint = 3
  def loadAddrMisaligned = 4
  def loadAccessFault = 5
  def storeAddrMisaligned = 6
  def storeAccessFault = 7
  def ecallU = 8
  def ecallS = 9
  def ecallM = 11
  def instrPageFault = 12
  def loadPageFault = 13
  // def singleStep          = 14
  def storePageFault = 15

  def priorities = Seq(
    breakPoint, // TODO: different BP has different priority
    instrPageFault,
    instrAccessFault,
    illegalInstr,
    instrAddrMisaligned,
    ecallM, ecallS, ecallU,
    storeAddrMisaligned,
    loadAddrMisaligned,
    storePageFault,
    loadPageFault,
    storeAccessFault,
    loadAccessFault
  )

  def all = priorities.distinct.sorted

  def frontendSet = Seq(
    instrAddrMisaligned,
    instrAccessFault,
    illegalInstr,
    instrPageFault
  )

  def partialSelect(vec: Vec[Bool], select: Seq[Int]): Vec[Bool] = {
    val new_vec = Wire(ExceptionVec())
    new_vec.foreach(_ := false.B)
    select.foreach(i => new_vec(i) := vec(i))
    new_vec
  }

  def selectFrontend(vec: Vec[Bool]): Vec[Bool] = partialSelect(vec, frontendSet)

  def selectAll(vec: Vec[Bool]): Vec[Bool] = partialSelect(vec, ExceptionNO.all)

  def selectByFu(vec: Vec[Bool], fuConfig: FuConfig): Vec[Bool] =
    partialSelect(vec, fuConfig.exceptionOut)

  def selectByExu(vec: Vec[Bool], exuConfig: ExuConfig): Vec[Bool] =
    partialSelect(vec, exuConfig.exceptionOut)

  def selectByExu(vec: Vec[Bool], exuConfigs: Seq[ExuConfig]): Vec[Bool] =
    partialSelect(vec, exuConfigs.map(_.exceptionOut).reduce(_ ++ _).distinct.sorted)
}

case class Imm_LUI_LOAD() {
  def immFromLuiLoad(lui_imm: UInt, load_imm: UInt): UInt = {
    val loadImm = load_imm(Imm_I().len - 1, 0)
    Cat(lui_imm(Imm_U().len - loadImm.getWidth - 1, 0), loadImm)
  }
  def getLuiImm(uop: MicroOp): UInt = {
    val loadImmLen = Imm_I().len
    val imm_u = Cat(uop.psrc(1), uop.psrc(0), uop.ctrl.imm(ImmUnion.maxLen - 1, loadImmLen))
    Imm_U().do_toImm32(imm_u)
  }
}

object RSFeedbackType {
  val tlbMiss = (1 << 3).U(7.W)
  val mshrFull = (1 << 3).U(7.W)
  val dataInvalid = (1 << 0).U(7.W)
  val bankConflict = (1 << 0).U(7.W)
  val ldVioCheckRedo = (1 << 0).U(7.W)
  val feedbackInvalid = (1 << 2).U(7.W)
  def apply() = UInt(7.W)
}


class RSFeedbackIO(bankNum:Int, entriesNum:Int) extends XSBundle{
  // Note: you need to update in implicit Parameters p before imp MemRSFeedbackIO
  // for instance: MemRSFeedbackIO()(updateP)
  val feedbackSlow = ValidIO(new RSFeedback(bankNum, entriesNum)) // dcache miss queue full, dtlb miss
  val feedbackFast = ValidIO(new RSFeedback(bankNum, entriesNum)) // bank conflict
  val isFirstIssue = Input(Bool())
}

class RSFeedback(bankNum:Int, entriesNum:Int) extends XSBundle {
  val rsIdx = new RsIdx(bankNum, entriesNum)
  val flushState = Bool()
  val sourceType = RSFeedbackType()
}


class SfenceBundle extends XSBundle {
  val valid = Bool()
  val bits = new Bundle {
    val rs1 = Bool()
    val rs2 = Bool()
    val addr = UInt(VAddrBits.W)
    val asid = UInt(AsidLength.W)
  }

  override def toPrintable: Printable = {
    p"valid:0x${Hexadecimal(valid)} rs1:${bits.rs1} rs2:${bits.rs2} addr:${Hexadecimal(bits.addr)}"
  }
}


class SatpStruct extends Bundle {
  val mode = UInt(4.W)
  val asid = UInt(16.W)
  val ppn  = UInt(44.W)
}

class TlbCsrBundle extends XSBundle {
  val satp = new Bundle {
    val changed = Bool()
    val mode = UInt(4.W) // TODO: may change number to parameter
    val asid = UInt(16.W)
    val ppn = UInt(44.W) // just use PAddrBits - 3 - vpnnLen

    def apply(satp_value: UInt): Unit = {
      require(satp_value.getWidth == XLEN)
      val sa = satp_value.asTypeOf(new SatpStruct)
      mode := sa.mode
      asid := sa.asid
      ppn := sa.ppn
      changed := DataChanged(sa.asid) // when ppn is changed, software need do the flush
    }
  }
  val priv = new Bundle {
    val mxr = Bool()
    val sum = Bool()
    val imode = UInt(2.W)
    val dmode = UInt(2.W)
  }

  override def toPrintable: Printable = {
    p"Satp mode:0x${Hexadecimal(satp.mode)} asid:0x${Hexadecimal(satp.asid)} ppn:0x${Hexadecimal(satp.ppn)} " +
      p"Priv mxr:${priv.mxr} sum:${priv.sum} imode:${priv.imode} dmode:${priv.dmode}"
  }
}

class FenceToSbuffer extends Bundle {
  val flushSb = Output(Bool())
  val sbIsEmpty = Input(Bool())
}

class LsqEnqIO extends XSBundle {
  val canAccept = Output(Bool())
  val needAlloc = Vec(4, Input(UInt(2.W)))      //todo ???
  val req = Vec(4, Flipped(ValidIO(new MicroOp)))
  val resp = Vec(4, Output(new LSIdx))
}

class LSIdx extends XSBundle {
  val lqIdx = new LqPtr
  val sqIdx = new SqPtr
}

class ExceptionAddrIO extends XSBundle {
  val isStore = Input(Bool())
  val vaddr = Output(UInt(VAddrBits.W))
}

class RobLsqIO extends XSBundle {
  val lcommit = Output(UInt(log2Up(CommitWidth + 1).W))
  val scommit = Output(UInt(log2Up(CommitWidth + 1).W))
  val pendingld = Output(Bool())
  val pendingst = Output(Bool())
  val commit = Output(Bool())
}


class CustomCSRCtrlIO extends XSBundle {
  // Prefetcher
  val l1I_pf_enable = Output(Bool())
  val l2_pf_enable = Output(Bool())
  val l1D_pf_enable = Output(Bool())
  val l1D_pf_train_on_hit = Output(Bool())
  val l1D_pf_enable_agt = Output(Bool())
  val l1D_pf_enable_pht = Output(Bool())
  val l1D_pf_active_threshold = Output(UInt(4.W))
  val l1D_pf_active_stride = Output(UInt(6.W))
  val l1D_pf_enable_stride = Output(Bool())
  val l2_pf_store_only = Output(Bool())
  // ICache
  val icache_parity_enable = Output(Bool())
  // Labeled XiangShan
  val dsid = Output(UInt(8.W)) // TODO: DsidWidth as parameter
  // Load violation predictor
  val lvpred_disable = Output(Bool())
  val no_spec_load = Output(Bool())
  val storeset_wait_store = Output(Bool())
  val storeset_no_fast_wakeup = Output(Bool())  ///more: not in memBlock RTL
  val lvpred_timeout = Output(UInt(5.W))
  // Branch predictor
  val bp_ctrl = Output(new BPUCtrl)
  // Memory Block
  val sbuffer_threshold = Output(UInt(4.W))
  val ldld_vio_check_enable = Output(Bool())
  val soft_prefetch_enable = Output(Bool())
  val cache_error_enable = Output(Bool())
  val ptw_prefercache_enable = Output(Bool())
  // Rename
  val fusion_enable = Output(Bool())
  val wfi_enable = Output(Bool())
  // Decode
  val svinval_enable = Output(Bool())
  val move_elim_enable = Output(Bool())

  // distribute csr write signal
  val distribute_csr = new DistributedCSRIO()
  // TODO: move it to a new bundle, since single step is not a custom control signal
  val singlestep = Output(Bool())
  val frontend_trigger = new FrontendTdataDistributeIO()
  val mem_trigger = new MemTdataDistributeIO()
}

class MemTdataDistributeIO extends XSBundle {
  val tUpdate = ValidIO(new Bundle {
    val addr = Output(UInt(log2Up(TriggerNum).W))
    val tdata = new MatchTriggerIO
  })
  val tEnableVec: Vec[Bool] = Output(Vec(TriggerNum, Bool()))
}

class BPUCtrl extends XSBundle {
  val ubtb_enable = Bool()
  val btb_enable  = Bool()
  val bim_enable  = Bool()
  val tage_enable = Bool()
  val sc_enable   = Bool()
  val ras_enable  = Bool()
  val loop_enable = Bool()
}

class DistributedCSRIO extends XSBundle {
  // CSR has been writen by csr inst, copies of csr should be updated
  val w = ValidIO(new Bundle {
    val addr = Output(UInt(12.W))
    val data = Output(UInt(XLEN.W))
  })
}


// these 3 bundles help distribute trigger control signals from CSR
// to Frontend, Load and Store.
class FrontendTdataDistributeIO extends XSBundle {
  val tUpdate = ValidIO(new Bundle {
    val addr = Output(UInt(log2Up(TriggerNum).W))
    val tdata = new MatchTriggerIO
  })
  val tEnableVec: Vec[Bool] = Output(Vec(TriggerNum, Bool()))
}

class MatchTriggerIO extends XSBundle {
  val matchType = Output(UInt(2.W))
  val select = Output(Bool())
  val timing = Output(Bool())
  val action = Output(Bool())
  val chain = Output(Bool())
  val execute = Output(Bool())
  val store = Output(Bool())
  val load = Output(Bool())
  val tdata2 = Output(UInt(64.W))
}

class DistributedCSRUpdateReq extends XSBundle {
  // Request csr to be updated
  //
  // Note that this request will ONLY update CSR Module it self,
  // copies of csr will NOT be updated, use it with care!
  //
  // For each cycle, no more than 1 DistributedCSRUpdateReq is valid
  val w = ValidIO(new Bundle {
    val addr = Output(UInt(12.W))
    val data = Output(UInt(XLEN.W))
  })
  def apply(valid: Bool, addr: UInt, data: UInt, src_description: String) = {
    when(valid){
      w.bits.addr := addr
      w.bits.data := data
    }
    println("Distributed CSR update req registered for " + src_description)
  }
}


class L1CacheErrorInfo extends XSBundle {
  // L1CacheErrorInfo is also used to encode customized CACHE_ERROR CSR
  val source = Output(new Bundle() {
    val tag = Bool() // l1 tag array
    val data = Bool() // l1 data array
    val l2 = Bool()
  })
  val opType = Output(new Bundle() {
    val fetch = Bool()
    val load = Bool()
    val store = Bool()
    val probe = Bool()
    val release = Bool()
    val atom = Bool()
  })
  val paddr = Output(UInt(PAddrBits.W))

  // report error and paddr to beu
  // bus error unit will receive error info iff ecc_error.valid
  val report_to_beu = Output(Bool())

  // there is an valid error
  // l1 cache error will always be report to CACHE_ERROR csr
  val valid = Output(Bool())

//  def toL1BusErrorUnitInfo(): L1BusErrorUnitInfo = {
//    val beu_info = Wire(new L1BusErrorUnitInfo)
//    beu_info.ecc_error.valid := report_to_beu
//    beu_info.ecc_error.bits := paddr
//    beu_info
//  }
}

class PerfEvent extends Bundle {
  val value = UInt(6.W)
}

class Ftq_RF_Components extends XSBundle{
  val startAddr = UInt(VAddrBits.W)
  val nextLineAddr = UInt(VAddrBits.W)
  val isNextMask = Vec(16, Bool())
  val fallThruError = Bool()

  def getPc(offset: UInt) = {
    def getHigher(pc: UInt) = pc(VAddrBits - 1, log2Ceil(PredictWidth) + instOffsetBits + 1)

    def getOffset(pc: UInt) = pc(log2Ceil(PredictWidth) + instOffsetBits, instOffsetBits)

    Cat(getHigher(Mux(isNextMask(offset) && startAddr(log2Ceil(PredictWidth) + instOffsetBits), nextLineAddr, startAddr)),
      getOffset(startAddr) + offset, 0.U(instOffsetBits.W))
  }
}