package regfile
import chisel3._
import chisel3.util._
import common.{ExuInput, ExuOutput, Redirect, XSParam}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.config.Parameters
import xs.utils.Assertion.xs_assert
class Regfile(writebackNum:Int, val entriesNum:Int, val typeString:String = "Int")(implicit p: Parameters) extends LazyModule with XSParam{
  require(typeString == "Int" || typeString == "Fp")
  val issueNode = new RegfileIssueNode
  val writeBackNode = new RegfileWriteBackNode(Seq.fill(writebackNum)(None))
  val isInt = typeString == "Int"
  override def module = new RegfileImpl(this)
}

class RegfileImpl(outer: Regfile)(implicit p: Parameters) extends LazyModuleImp(outer){
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
  })
  private val issueIn = outer.issueNode.in
  private val issueOut = outer.issueNode.out
  private val writeBacks = outer.writeBackNode.in
  private val mem = Mem(outer.entriesNum, UInt(64.W))
  private val wbEnables = writeBacks.map({case(b, _) =>
    val correctRf = if(outer.isInt) b.bits.uop.ctrl.rfWen else b.bits.uop.ctrl.fpWen
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
    val outBundle = Wire(Valid(new ExuInput(eo.srcNum)))
    outBundle.valid := bi.valid
    outBundle.bits.uop := bi.bits
    outBundle.bits.src.take(eo.srcNum).zip(bi.bits.psrc.take(eo.srcNum)).foreach({case(data, addr) =>
      val bypassOH = wbsWithBypass.map(_._1.bits.uop.pdest).zip(wbEnables).map({case(dst, en)=> en & dst === addr})
      val bypassData = Mux1H(bypassOH, writeBacks.map(_._1.bits.data))
      val bypassValid = Cat(bypassOH).orR
      data := Mux(bypassValid, bypassData, mem(addr))
      xs_assert(PopCount(bypassOH) === 1.U)
    })
    bo.valid := RegNext(outBundle.valid, false.B)
    bo.bits := RegEnable(outBundle.bits, outBundle.valid)
  })
}