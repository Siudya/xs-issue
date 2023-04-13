package regfile
import chisel3._
import chisel3.util._
import common.{ExuInput, FuType, Redirect, XSParam}
import exu.ExuType
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.config.Parameters
import writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}
import xs.utils.Assertion.xs_assert
class Regfile(val entriesNum:Int, val typeString:String = "Int", name:String)(implicit p: Parameters) extends LazyModule with XSParam{
  require(typeString == "Int" || typeString == "Fp")
  val isInt = typeString == "Int"
  private val wbNodeParam = WriteBackSinkParam(name, if(isInt) WriteBackSinkType.intRf else WriteBackSinkType.fpRf)
  val issueNode = new RegfileIssueNode
  val writeBackNode = new WriteBackSinkNode(wbNodeParam)
  lazy val module = new RegfileImpl(this)
}

class RegfileImpl(outer: Regfile)(implicit p: Parameters) extends LazyModuleImp(outer) with XSParam{
  private val isInt = outer.isInt
  private val issueIn = outer.issueNode.in
  private val issueOut = outer.issueNode.out
  private val jmpNum = issueOut.map(_._2).count(_.exuType == ExuType.jmp)
  require(jmpNum <= 1, "Only one jmp module is supported!")
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val jmpTargetRead = Output(UInt(if(jmpNum > 0) log2Ceil(FtqSize).W else 0.W))
    val jmpTargetData = Input(UInt(if(jmpNum > 0) VAddrBits.W else 0.W))
    val jmpPcRead = Output(UInt(if(jmpNum > 0) log2Ceil(FtqSize).W else 0.W))
    val jmpPcData = Input(UInt(if(jmpNum > 0) VAddrBits.W else 0.W))
  })
  private val wbIn = outer.writeBackNode.in.head
  private val writeBacks = wbIn._1.zip(wbIn._2)
  private val mem = Mem(outer.entriesNum, UInt(XLEN.W))
  private val wbEnables = writeBacks.map({case(b, _) =>
    val correctRf = if(isInt) b.bits.uop.ctrl.rfWen else b.bits.uop.ctrl.fpWen
    correctRf && b.valid && !b.bits.uop.robIdx.needFlush(io.redirect)
  })
  writeBacks.zip(wbEnables).foreach({
    case((b, _), en) =>
      when(en){
        mem(b.bits.uop.pdest) := b.bits.data
      }
  })
  private val wbsWithBypass = writeBacks.filter(_._2.hasFastWakeup)
  issueIn.zip(issueOut).foreach({case(issI, issO) =>
    val bi = issI._1
    val bo = issO._1
    val eo = issO._2
    bi.release := bo.release
    val outBundle = Wire(new ExuInput(eo.releaseWidth))
    outBundle.release := DontCare
    outBundle.fuInput.valid := bi.uop.valid
    outBundle.fuInput.bits.uop := bi.uop.bits
    outBundle.fuInput.bits.src.take(eo.srcNum).zip(bi.uop.bits.psrc.take(eo.srcNum)).foreach({case(data, addr) =>
      val bypassOH = wbsWithBypass.map(_._1.bits.uop.pdest).zip(wbEnables).map({case(dst, en)=> en & dst === addr})
      val bypassData = Mux1H(bypassOH, writeBacks.map(_._1.bits.data))
      val bypassValid = Cat(bypassOH).orR
      data := Mux(bypassValid, bypassData, mem(addr))
      xs_assert(PopCount(bypassOH) === 1.U)
    })
    if(eo.srcNum < outBundle.fuInput.bits.src.length) outBundle.fuInput.bits.src.slice(eo.srcNum, outBundle.fuInput.bits.src.length).foreach(_ := DontCare)
    if(eo.exuType == ExuType.jmp){
      io.jmpTargetRead := (bi.uop.bits.cf.ftqPtr + 1.U).value
      io.jmpPcRead := bi.uop.bits.cf.ftqPtr.value
      bo.fuInput.bits.uop.cf.pc := io.jmpPcData
      when(bi.uop.bits.ctrl.fuType === FuType.jmp) {
        bo.fuInput.bits.src(1) := io.jmpTargetData
      }
    }
    val outputValidDriverRegs = RegNext(outBundle.fuInput.valid, false.B)
    val outputDataDriverRegs = RegEnable(outBundle.fuInput.bits, outBundle.fuInput.valid)
    bo.fuInput.valid := outputValidDriverRegs
    bo.fuInput.bits := outputDataDriverRegs
  })
}