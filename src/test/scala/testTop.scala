import chisel3.stage.ChiselGeneratorAnnotation
import freechips.rocketchip.diplomacy._
import xs.utils.Assertion
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import common.{ExuOutput, MicroOp, Redirect}
import regfile.{PcMem, PcWritePort, RegFileTop}
import chisel3._
import chisel3.util._
import execute.exu.FenceIO
import execute.exublock.{FloatingBlock, IntegerBlock, MemoryBlock}
import issue.FpRs.FloatingReservationStation
import issue.IntRs.IntegerReservationStation
import issue.MemRs.MemoryReservationStation
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

class TestTop(implicit p:Parameters) extends LazyModule{
  private val pcMemEntries = 64
  private val integerBlock = LazyModule(new IntegerBlock(2, 1, 1, 1))
  private val floatingBlock = LazyModule(new FloatingBlock(2, 1, 1))
  private val memoryBlock = LazyModule(new MemoryBlock(2, 2, 1))
  private val integerReservationStation = LazyModule(new IntegerReservationStation(4, 16))
  private val floatingReservationStation = LazyModule(new FloatingReservationStation(4, 16))
  private val memoryReservationStation = LazyModule(new MemoryReservationStation(4, 16, 6))
  private val writebackNetwork = LazyModule(new WriteBackNetwork)
  private val regFile = LazyModule(new RegFileTop)
  private val exuBlocks = integerBlock :: floatingBlock :: memoryBlock :: Nil

  regFile.issueNode :*= integerReservationStation.issueNode
  regFile.issueNode :*= floatingReservationStation.issueNode
  regFile.issueNode :*= memoryReservationStation.issueNode
  for (eb <- exuBlocks) {
    eb.issueNode :*= regFile.issueNode
    writebackNetwork.node :=* eb.writebackNode
  }
  regFile.writebackNode :=* writebackNetwork.node
  floatingReservationStation.wakeupNode := writebackNetwork.node
  integerReservationStation.wakeupNode := writebackNetwork.node
  memoryReservationStation.wakeupNode := writebackNetwork.node

  lazy val module = new LazyModuleImp(this){
    private val redirectOutNum = writebackNetwork.node.in.count(_._2.hasRedirectOut)
    val io = IO(new Bundle {
      val redirectIn = Input(Valid(new Redirect))
      val enqInt = Vec(4, Flipped(DecoupledIO(new MicroOp)))
      val enqFp = Vec(4, Flipped(DecoupledIO(new MicroOp)))
      val enqMem = Vec(4, Flipped(DecoupledIO(new MicroOp)))
      val redirectOut = Output(Vec(redirectOutNum, Valid(new Redirect)))
      val fenceio = new FenceIO
      val pcMemWrite = new PcWritePort(log2Ceil(pcMemEntries))
    })
    exuBlocks.foreach(_.module.redirectIn := io.redirectIn)

    integerReservationStation.module.io.redirect := io.redirectIn
    integerReservationStation.module.io.enq <> io.enqInt
    integerReservationStation.module.io.loadEarlyWakeup := memoryReservationStation.module.io.loadEarlyWakeup
    integerReservationStation.module.io.earlyWakeUpCancel := memoryBlock.module.io.earlyWakeUpCancel

    floatingReservationStation.module.io.redirect := io.redirectIn
    floatingReservationStation.module.io.enq <> io.enqFp
    floatingReservationStation.module.io.loadEarlyWakeup := memoryReservationStation.module.io.loadEarlyWakeup
    floatingReservationStation.module.io.earlyWakeUpCancel := memoryBlock.module.io.earlyWakeUpCancel

    memoryReservationStation.module.io.redirect := io.redirectIn
    memoryReservationStation.module.io.enq <> io.enqMem
    memoryReservationStation.module.io.specWakeup := integerReservationStation.module.io.specWakeup
    memoryReservationStation.module.io.earlyWakeUpCancel := memoryBlock.module.io.earlyWakeUpCancel

    private val pcMem = Module(new PcMem(pcMemEntries, regFile.module.pcReadNum, 1))
    pcMem.io.write.head := io.pcMemWrite

    pcMem.io.read.zip(regFile.module.io.pcReadAddr).foreach({case(r, addr) => r.addr := addr})
    regFile.module.io.pcReadData.zip(pcMem.io.read).foreach({case(data, r) => data := r.data})

    integerBlock.module.io.fenceio <> io.fenceio

    writebackNetwork.module.io.redirectIn := io.redirectIn
    io.redirectOut.zip(writebackNetwork.module.io.redirectOut).foreach({case(sink, src) =>
      sink.valid := src.bits.redirectValid
      sink.bits := src.bits.redirect
    })
  }
}

object GenRtl extends App {
  Assertion.set_enable(false)
  implicit val p: MyConfig = new MyConfig
  val top = LazyModule(new TestTop)
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
  writeOutputFile("./build", s"TestTop.graphml", top.graphML)
}