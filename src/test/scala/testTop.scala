import chisel3.stage.ChiselGeneratorAnnotation
import exu.{AluExu, DivExu, FmacExu, FmiscExu, JmpCsrExu, MulExu}
import freechips.rocketchip.diplomacy._
import xs.utils.Assertion
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import common.{ExuOutput, MicroOp, Redirect}
import regfile.{FloatingRegFile, IntegerRegFile}
import chisel3._
import chisel3.util._
import issue.FpRs.FloatingReservationStation
import issue.IntRs.IntegerReservationStation
import issue.RsIssueNode
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
  private val integerReservationStation = LazyModule(new IntegerReservationStation(4))
  private val writebackNetwork = LazyModule(new WriteBackNetwork)
  private val intRegFile = LazyModule(new IntegerRegFile(32, "Integer Regfile"))
  private val alus = Seq.tabulate(4)(idx => LazyModule(new AluExu(idx, 0)))
  private val muls = Seq.tabulate(2)(idx => LazyModule(new MulExu(idx, 0)))
  private val divs = Seq.tabulate(2)(idx => LazyModule(new DivExu(idx, 0)))
  private val jmps = Seq.tabulate(1)(idx => LazyModule(new JmpCsrExu(idx, 0)))
  private val exus = alus ++ muls ++ divs ++ jmps

  intRegFile.issueNode :*= integerReservationStation.issueNode
  for(exu <- exus){
    exu.issueNode :*= intRegFile.issueNode
    writebackNetwork.node :=* exu.writebackNode
  }
  intRegFile.writeBackNode :=* writebackNetwork.node
  integerReservationStation.wakeupNode := writebackNetwork.node

  lazy val module = new LazyModuleImp(this){
    private val redirectOutNum = writebackNetwork.node.in.count(_._2.hasRedirectOut)
    val io = IO(new Bundle {
      val redirectIn = Input(Valid(new Redirect))
      val enq = Vec(4, Flipped(DecoupledIO(new MicroOp)))
      val redirectOut = Output(Vec(redirectOutNum, Valid(new Redirect)))
    })

    exus.foreach(_.module.redirectIn := io.redirectIn)
    intRegFile.module.io.redirect := io.redirectIn
    intRegFile.module.io.jmpTargetData := DontCare
    intRegFile.module.io.jmpPcData := DontCare
    writebackNetwork.module.io.redirectIn := io.redirectIn
    integerReservationStation.module.io.redirect := io.redirectIn
    integerReservationStation.module.io.enq <> io.enq
    io.redirectOut := writebackNetwork.module.io.redirectOut
    jmps.head.module.io.fenceio.sbuffer.sbIsEmpty := true.B
  }
}

class FloatingTop(implicit p:Parameters) extends LazyModule{
  private val floatingReservationStation = LazyModule(new FloatingReservationStation(4))
  private val writebackNetwork = LazyModule(new WriteBackNetwork)
  private val fpRegFile = LazyModule(new FloatingRegFile(32, "Floating Regfile"))
  private val fmiscs = Seq.tabulate(1)(idx => LazyModule(new FmiscExu(idx)))
  private val fmacs = Seq.tabulate(2)(idx => LazyModule(new FmacExu(idx)))
  private val exus = fmiscs ++ fmacs

  fpRegFile.issueNode :*= floatingReservationStation.issueNode
  for(exu <- exus){
    exu.issueNode :*= fpRegFile.issueNode
    writebackNetwork.node :=* exu.writebackNode
  }
  fpRegFile.writeBackNode :=* writebackNetwork.node
  floatingReservationStation.wakeupNode := writebackNetwork.node

  lazy val module = new LazyModuleImp(this){
    private val redirectOutNum = writebackNetwork.node.in.count(_._2.hasRedirectOut)
    val io = IO(new Bundle {
      val redirectIn = Input(Valid(new Redirect))
      val enq = Vec(4, Flipped(DecoupledIO(new MicroOp)))
      val redirectOut = Output(Vec(redirectOutNum, Valid(new Redirect)))
    })

    exus.foreach(_.module.redirectIn := io.redirectIn)
    fpRegFile.module.io.redirect := io.redirectIn
    writebackNetwork.module.io.redirectIn := io.redirectIn
    floatingReservationStation.module.io.redirect := io.redirectIn
    floatingReservationStation.module.io.enq <> io.enq
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
  writeOutputFile("./build", s"FakeTop.graphml", top.graphML)
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
  writeOutputFile("./build", s"FakeTop.graphml", top.graphML)
}