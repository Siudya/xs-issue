package exu
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import common.{ExuOutput, FuType, Redirect, XSParam}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, LazyModuleImpLike}
import fu.FuConfigs
import fu.bku.Bku
import fu.mdu.{ArrayMultiplier, MDUOpType}
import xs.utils.Assertion.xs_assert
import xs.utils.{LookupTree, ParallelMux, SignExt, ZeroExt}

class MulExu(id:Int, val bypassInNum:Int)(implicit p:Parameters) extends LazyModule{
  private val cfg  = ExuConfig(
    name = "MulExu",
    id = id,
    blockName = "IntegerBlock",
    fuConfigs = Seq(FuConfigs.mulCfg, FuConfigs.bkuCfg),
    exuType = ExuType.mul,
    srcNum = 2
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutNode(cfg)

  lazy val module = new MulExuImpl(this)
}
class MulExuImpl(outer:MulExu)(implicit p:Parameters) extends LazyModuleImp(outer) with XSParam{
  val io = IO(new Bundle{
    val redirectIn = Input(Valid(new Redirect))
    val bypassIn = Input(Vec(outer.bypassInNum, Valid(new ExuOutput)))
    val bypassOut = Output(Valid(new ExuOutput))
  })
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1

  private val mul = Module(new ArrayMultiplier(XLEN + 1)(p))
  private val bku = Module(new Bku)

  private val bypass = io.bypassIn :+ writebackPort
  private val bypassData = bypass.map(_.bits.data)
  private val bypassSrc0Hits = bypass.map(elm => elm.valid && elm.bits.uop.pdest === issuePort.fuInput.bits.uop.psrc(0))
  private val bypassSrc1Hits = bypass.map(elm => elm.valid && elm.bits.uop.pdest === issuePort.fuInput.bits.uop.psrc(1))
  private val bypassSrc0Valid = Cat(bypassSrc0Hits).orR
  private val bypassSrc0Data = Mux1H(bypassSrc0Hits, bypassData)
  private val bypassSrc1Valid = Cat(bypassSrc1Hits).orR
  private val bypassSrc1Data = Mux1H(bypassSrc1Hits, bypassData)

  private val finalIssueSignals = WireInit(issuePort.fuInput)
  finalIssueSignals.bits.src(0) := Mux(bypassSrc0Valid, bypassSrc0Data, issuePort.fuInput.bits.src(0))
  finalIssueSignals.bits.src(1) := Mux(bypassSrc1Valid, bypassSrc1Data, issuePort.fuInput.bits.src(1))

  bku.io.in.valid := finalIssueSignals.valid && finalIssueSignals.bits.uop.ctrl.fuType === FuType.bku
  bku.io.in.bits.uop := finalIssueSignals.bits.uop
  bku.io.in.bits.src(0) := finalIssueSignals.bits.src(0)
  bku.io.in.bits.src(1) := finalIssueSignals.bits.src(1)
  bku.io.in.bits.src(2) := DontCare
  bku.io.redirectIn := io.redirectIn

  mul.io.in.valid := finalIssueSignals.valid && finalIssueSignals.bits.uop.ctrl.fuType === FuType.mul
  mul.io.in.bits.uop := finalIssueSignals.bits.uop
  mul.io.in.bits.src(0) := finalIssueSignals.bits.src(0)
  mul.io.in.bits.src(1) := finalIssueSignals.bits.src(1)
  mul.io.in.bits.src(2) := DontCare
  mul.io.redirectIn := io.redirectIn

  private val (func, src1, src2) = (
    finalIssueSignals.bits.uop.ctrl.fuOpType,
    finalIssueSignals.bits.src(0)(XLEN - 1, 0),
    finalIssueSignals.bits.src(1)(XLEN - 1, 0)
  )
  private val op = MDUOpType.getMulOp(func)
  private val signext = SignExt(_: UInt, XLEN + 1)
  private val zeroext = ZeroExt(_: UInt, XLEN + 1)
  private val mulInputFuncTable = List(
    MDUOpType.mul -> (zeroext, zeroext),
    MDUOpType.mulh -> (signext, signext),
    MDUOpType.mulhsu -> (signext, zeroext),
    MDUOpType.mulhu -> (zeroext, zeroext)
  )

  mul.io.in.bits.src(0) := LookupTree(
    op,
    mulInputFuncTable.map(p => (p._1(1, 0), p._2._1(src1)))
  )
  when(func(3)) {
    mul.io.in.bits.src(0) := src1(6, 0)
  }
  mul.io.in.bits.src(1) := LookupTree(
    op,
    mulInputFuncTable.map(p => (p._1(1, 0), p._2._2(src2)))
  )

  private val isW = MDUOpType.isW(func)
  private val isH = MDUOpType.isH(func)
  mul.ctrl.isW := isW
  mul.ctrl.isHi := isH
  mul.ctrl.sign := DontCare

  private val outSel = Seq(mul.io.out.valid, bku.io.out.valid)
  private val outData = Seq(mul.io.out, bku.io.out)
  private val finalData = ParallelMux(outSel, outData)
  writebackPort := DontCare
  writebackPort.valid := finalData.valid
  writebackPort.bits.uop := finalData.bits.uop
  writebackPort.bits.data := finalData.bits.data
  io.bypassOut := writebackPort

//  xs_assert(PopCount(Cat(outSel)) === 1.U || Cat(outSel) === 0.U)
}
