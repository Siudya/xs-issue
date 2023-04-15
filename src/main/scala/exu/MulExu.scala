package exu
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import common.{ExuOutput, FuType, XSParam}
import fu.FuConfigs
import fu.bku.Bku
import fu.mdu.{ArrayMultiplier, MDUOpType}
import xs.utils.Assertion.xs_assert
import xs.utils.{LookupTree, ParallelMux, SignExt, ZeroExt}

class MulExu(id:Int, val bypassInNum:Int)(implicit p:Parameters) extends BasicExu{
  private val cfg  = ExuConfig(
    name = "MulExu",
    id = id,
    blockName = "IntegerBlock",
    fuConfigs = Seq(FuConfigs.mulCfg, FuConfigs.bkuCfg),
    exuType = ExuType.mul
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutNode(cfg)

  lazy val module = new MulExuImpl(this)
}
class MulExuImpl(outer:MulExu)(implicit p:Parameters) extends BasicExuImpl(outer) with XSParam{
  val io = IO(new Bundle{
    val bypassIn = Input(Vec(outer.bypassInNum, Valid(new ExuOutput)))
    val bypassOut = Output(Valid(new ExuOutput))
  })
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1

  private val mul = Module(new ArrayMultiplier(XLEN + 1)(p))
  private val bku = Module(new Bku)

  issuePort.issue.ready := true.B
  private val finalIssueSignals = bypassSigGen(io.bypassIn :+ writebackPort, issuePort, outer.bypassInNum > 0)

  bku.io.in.valid := finalIssueSignals.valid && finalIssueSignals.bits.uop.ctrl.fuType === FuType.bku
  bku.io.in.bits.uop := finalIssueSignals.bits.uop
  bku.io.in.bits.src := finalIssueSignals.bits.src
  bku.io.redirectIn := redirectIn
  bku.io.out.ready := true.B

  mul.io.in.valid := finalIssueSignals.valid && finalIssueSignals.bits.uop.ctrl.fuType === FuType.mul
  mul.io.in.bits.uop := finalIssueSignals.bits.uop
  mul.io.in.bits.src(2) := DontCare
  mul.io.redirectIn := redirectIn
  mul.io.out.ready := true.B

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
}
