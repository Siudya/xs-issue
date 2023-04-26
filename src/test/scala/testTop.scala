import chisel3.stage.ChiselGeneratorAnnotation
import execute.exu.{AluExu, DivExu, FmacExu, FmiscExu, JmpCsrExu, MulExu}
import freechips.rocketchip.diplomacy._
import xs.utils.Assertion
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import common.{ExuOutput, MicroOp, Redirect}
import regfile.{FloatingRegFile, IntegerRegFile}
import chisel3._
import chisel3.util._
import execute.exucx._
import issue.FpRs.FloatingReservationStation
import issue.IntRs.IntegerReservationStation
import issue.{EarlyWakeUpInfo, RsIssueNode}
import writeback.WriteBackNetwork

import java.io.{File, FileWriter}
case object ParamKey extends Field[Param]
case class Param
(
  XLEN:Int = 64
)
class MyConfig extends Config((site, here, up) => {
  case ParamKey => Param()
})

class IntegerTop(implicit p:Parameters) extends LazyModule{
  private val integerReservationStation = LazyModule(new IntegerReservationStation(4, 16))
  private val writebackNetwork = LazyModule(new WriteBackNetwork)
  private val intRegFile = LazyModule(new IntegerRegFile(32, "Integer Regfile"))
  private val aluMuls = Seq.tabulate(2)(idx => LazyModule(new AluMulComplex(idx, 1)))
  private val aluDivs = Seq.tabulate(1)(idx => LazyModule(new AluDivComplex(idx, 2)))
  private val aluI2fs = Seq.tabulate(1)(idx => LazyModule(new AluI2fComplex(idx, 2)))
  private val jmps = Seq.tabulate(1)(idx => LazyModule(new JmpCsrComplex(idx, 2)))
  private val complexes = jmps ++ aluMuls ++ aluDivs ++ aluI2fs

  intRegFile.issueNode :*= integerReservationStation.issueNode
  for(cplx <- complexes){
    cplx.issueNode :*= intRegFile.issueNode
    writebackNetwork.node :=* cplx.writebackNode
  }
  intRegFile.writeBackNode :=* writebackNetwork.node
  integerReservationStation.wakeupNode := writebackNetwork.node

  lazy val module = new LazyModuleImp(this){
    private val redirectOutNum = writebackNetwork.node.in.count(_._2.hasRedirectOut)
    val io = IO(new Bundle {
      val redirectIn = Input(Valid(new Redirect))
      val enq = Vec(4, Flipped(DecoupledIO(new MicroOp)))
      val redirectOut = Output(Vec(redirectOutNum, Valid(new Redirect)))
      val loadEarlyWakeup = Input(Vec(2, Valid(new EarlyWakeUpInfo)))
      val earlyWakeUpCancel = Input(Vec(2, Bool()))
    })

    complexes.foreach(_.module.redirectIn := io.redirectIn)

    (jmps ++ aluDivs ++ aluI2fs).foreach(cplx => cplx.module.bypassIn.zip(aluMuls.map(_.module.io.bypassOut)).foreach({case(a, b) => a := b}))
    aluMuls.map(_.module.bypassIn).zipWithIndex.foreach({case(bin,idx) =>
      val sources = aluMuls.zipWithIndex.filterNot(_._2 == idx).map(_._1.module.io.bypassOut)
      bin.zip(sources).foreach({case(a, b) => a := b})
    })
    intRegFile.module.io.redirect := io.redirectIn
    intRegFile.module.io.jmpTargetData := DontCare
    intRegFile.module.io.jmpPcData := DontCare
    writebackNetwork.module.io.redirectIn := io.redirectIn
    integerReservationStation.module.io.redirect := io.redirectIn
    integerReservationStation.module.io.enq <> io.enq
    integerReservationStation.module.io.loadEarlyWakeup := io.loadEarlyWakeup
    integerReservationStation.module.io.earlyWakeUpCancel := io.earlyWakeUpCancel
    io.redirectOut := writebackNetwork.module.io.redirectOut
    jmps.head.module.io.fenceio.sbuffer.sbIsEmpty := true.B
  }
}

class FloatingTop(implicit p:Parameters) extends LazyModule{
  private val floatingReservationStation = LazyModule(new FloatingReservationStation(4,16))
  private val writebackNetwork = LazyModule(new WriteBackNetwork)
  private val fpRegFile = LazyModule(new FloatingRegFile(32, "Floating Regfile"))
  private val fmacs = Seq.tabulate(2)(idx => LazyModule(new FmacComplex(idx)))
  private val fmacDivs = Seq.tabulate(1)(idx => LazyModule(new FmaDivComplex(idx)))
  private val fmaMiscs = Seq.tabulate(1)(idx => LazyModule(new FmaMiscComplex(idx)))
  private val complexes = fmacs ++ fmacDivs ++ fmaMiscs

  fpRegFile.issueNode :*= floatingReservationStation.issueNode
  for(cplx <- complexes){
    cplx.issueNode :*= fpRegFile.issueNode
    writebackNetwork.node :=* cplx.writebackNode
  }
  fpRegFile.writeBackNode :=* writebackNetwork.node
  floatingReservationStation.wakeupNode := writebackNetwork.node

  lazy val module = new LazyModuleImp(this){
    private val redirectOutNum = writebackNetwork.node.in.count(_._2.hasRedirectOut)
    val io = IO(new Bundle {
      val redirectIn = Input(Valid(new Redirect))
      val enq = Vec(4, Flipped(DecoupledIO(new MicroOp)))
      val redirectOut = Output(Vec(redirectOutNum, Valid(new Redirect)))
      val loadEarlyWakeup = Input(Vec(2, Valid(new EarlyWakeUpInfo)))
      val earlyWakeUpCancel = Input(Vec(2, Bool()))
    })

    complexes.foreach(_.module.redirectIn := io.redirectIn)
    fpRegFile.module.io.redirect := io.redirectIn
    writebackNetwork.module.io.redirectIn := io.redirectIn
    floatingReservationStation.module.io.redirect := io.redirectIn
    floatingReservationStation.module.io.enq <> io.enq
    floatingReservationStation.module.io.loadEarlyWakeup := io.loadEarlyWakeup
    floatingReservationStation.module.io.earlyWakeUpCancel := io.earlyWakeUpCancel
    io.redirectOut := writebackNetwork.module.io.redirectOut
  }
}

object GenIntRtl extends App {
  Assertion.set_enable(false)
  implicit val p: MyConfig = new MyConfig
  val top = LazyModule(new IntegerTop)
  (new chisel3.stage.ChiselStage).execute(args,
    Seq(
      ChiselGeneratorAnnotation(() => top.module)
    )
  )

  def writeOutputFile(targetDir: String, fname: String, contents: String): File = {
    val f = new File(targetDir, fname)
    val fw = new FileWriter(f)
    fw.write(contents)
    fw.close
    f
  }
  writeOutputFile("./build", s"IntegerTop.graphml", top.graphML)
}

object GenFpRtl extends App {
  Assertion.set_enable(false)
  implicit val p: MyConfig = new MyConfig
  val top = LazyModule(new FloatingTop)
  (new chisel3.stage.ChiselStage).execute(args,
    Seq(
      ChiselGeneratorAnnotation(() => top.module)
    )
  )

  def writeOutputFile(targetDir: String, fname: String, contents: String): File = {
    val f = new File(targetDir, fname)
    val fw = new FileWriter(f)
    fw.write(contents)
    fw.close
    f
  }
  writeOutputFile("./build", s"FloatingTop.graphml", top.graphML)
}