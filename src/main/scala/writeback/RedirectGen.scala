package writeback
import chisel3._
import chisel3.util._
import common.{ExuOutput, Ftq_RF_Components, Redirect, RobPtr, XSModule}
import RedirectGen._
import xs.utils.ParallelOperation

class RedirectSelectBundle(idxWidth:Int) extends Bundle{
  val robIdx = new RobPtr
  val idxOH = UInt(idxWidth.W)
}

object RedirectGen{
  private def getRedirect(exuOut: Valid[ExuOutput]): ValidIO[Redirect] = {
    val redirect = Wire(Valid(new Redirect))
    redirect.valid := exuOut.valid
    redirect.bits := exuOut.bits.redirect
    redirect
  }
  private def oldestSelect(in0:Valid[RedirectSelectBundle], in1:Valid[RedirectSelectBundle], idxWidth:Int):Valid[RedirectSelectBundle] = {
    val res = Wire(Valid(new RedirectSelectBundle(idxWidth)))
    res.valid := in0.valid | in1.valid
    res.bits := MuxLookup(Cat(in1.valid, in0.valid), in0.bits, Seq(
      "b10".U -> in1.bits,
      "b11".U -> Mux(in0.bits.robIdx < in1.bits.robIdx, in0.bits, in1.bits)
    ))
    res
  }

  def selectOldestRedirect(in: Seq[Valid[ExuOutput]]): (Valid[ExuOutput], UInt) = {
    val idxWidth = in.length
    val selectInfo = in.zipWithIndex.map({case(r, idx) =>
      val res = Wire(Valid(new RedirectSelectBundle(idxWidth)))
      res.valid := r.valid
      res.bits.robIdx := r.bits.uop.robIdx
      res.bits.idxOH := 1.U << idx
      res
    })
    val op = oldestSelect(_, _, idxWidth)
    val selRes = ParallelOperation(selectInfo, op)
    val res = Wire(Valid(new ExuOutput))
    res.valid := selRes.valid
    res.bits := Mux1H(selRes.bits.idxOH, in.map(_.bits))
    (res, selRes.bits.idxOH)
  }
}

class RedirectGen(jmpRedirectNum:Int, aluRedirectNum:Int, lsRedirectNum:Int, llRedirectNum:Int) extends XSModule{
  require(jmpRedirectNum == 1)
  val io = IO(new Bundle{
    val jmpWbIn = Input(Vec(jmpRedirectNum, Flipped(ValidIO(new ExuOutput))))
    val aluWbIn = Input(Vec(aluRedirectNum, Flipped(ValidIO(new ExuOutput))))
    val memWbIn = Input(Vec(lsRedirectNum, Flipped(ValidIO(new ExuOutput))))
    val pcReadAddr = Output(UInt(log2Ceil(FtqSize).W))
    val pcReadData = Input(new Ftq_RF_Components)
    val flush = Input(Bool())
    val redirectOut = Output(Valid(new Redirect))
  })

  private val allWb = io.jmpWbIn ++ io.aluWbIn ++ io.memWbIn
  private val allRedirect = allWb.map(getRedirect)
  private val (exuOutSel, redirectIdxOH) = selectOldestRedirect(allWb)
  private val redirectSel = Mux1H(redirectIdxOH, allRedirect)
  private val redirectValid = exuOutSel.valid && !io.flush

  private var addrIdx = 0
  private val isJmp = redirectIdxOH(jmpRedirectNum + addrIdx, addrIdx).orR
  addrIdx = addrIdx + jmpRedirectNum
  private val isAlu = redirectIdxOH(aluRedirectNum + addrIdx, addrIdx).orR
  addrIdx = addrIdx + aluRedirectNum
  private val isMem = redirectIdxOH(lsRedirectNum + addrIdx, addrIdx).orR
  addrIdx = addrIdx + lsRedirectNum

  io.pcReadAddr := exuOutSel.bits.uop.cf.ftqPtr
  private val s1_pcReadReg = RegEnable(io.pcReadData.getPc(exuOutSel.bits.uop.cf.ftqOffset), redirectValid)
  private val s1_jmpTargetReg = RegEnable(io.jmpWbIn.head.bits.uop.cf.pc, redirectValid)
  private val s1_imm12Reg = RegEnable(exuOutSel.bits.uop.ctrl.imm(11, 0), redirectValid)
  private val s1_pdReg = RegEnable(exuOutSel.bits.uop.cf.pd, redirectValid)
  private val s1_redirectIdxOH = RegEnable(redirectIdxOH, redirectValid)
  private val s1_redirectBitsReg = RegEnable(redirectSel, redirectValid)
  private val s1_redirectValidReg = RegNext(redirectValid, false.B)

}
