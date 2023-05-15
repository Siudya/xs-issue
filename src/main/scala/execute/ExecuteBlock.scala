package execute
import chisel3._
import chisel3.util._
import execute.exublock.{FloatingBlock, IntegerBlock, MemoryBlock}
import issue.FpRs.FloatingReservationStation
import issue.IntRs.IntegerReservationStation
import issue.MemRs.MemoryReservationStation
import chipsalliance.rocketchip.config.{Parameters}
import common.{MicroOp, Redirect}
import execute.exu.FenceIO
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import regfile.{PcMem, PcWritePort, RegFileTop}
import writeback.WriteBackNetwork
class ExecuteBlock(implicit p:Parameters) extends LazyModule {
  private val pcMemEntries = 64
  private val integerReservationStation = LazyModule(new IntegerReservationStation(4, 16))
  private val floatingReservationStation = LazyModule(new FloatingReservationStation(4, 16))
  private val memoryReservationStation = LazyModule(new MemoryReservationStation(4, 16, 6))
  private val integerBlock = LazyModule(new IntegerBlock(2, 1, 1))
  private val floatingBlock = LazyModule(new FloatingBlock(2, 1, 1))
  private val memoryBlock = LazyModule(new MemoryBlock(2, 2))
  private val regFile = LazyModule(new RegFileTop)
  private val writebackNetwork = LazyModule(new WriteBackNetwork)
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
    val io = IO(new Bundle {
      val enqInt = Vec(4, Flipped(DecoupledIO(new MicroOp)))
      val enqFp = Vec(4, Flipped(DecoupledIO(new MicroOp)))
      val enqMem = Vec(4, Flipped(DecoupledIO(new MicroOp)))
      val redirectOut = Output(Valid(new Redirect))
      val fenceio = new FenceIO
      val pcMemWrite = new PcWritePort(log2Ceil(pcMemEntries))
    })
    private val localRedirect = writebackNetwork.module.io.redirectOut
    exuBlocks.foreach(_.module.redirectIn := Pipe(localRedirect))

    integerReservationStation.module.io.redirect := Pipe(localRedirect)
    integerReservationStation.module.io.enq <> io.enqInt
    integerReservationStation.module.io.loadEarlyWakeup := memoryReservationStation.module.io.loadEarlyWakeup
    integerReservationStation.module.io.earlyWakeUpCancel := memoryBlock.module.io.earlyWakeUpCancel

    floatingReservationStation.module.io.redirect := Pipe(localRedirect)
    floatingReservationStation.module.io.enq <> io.enqFp
    floatingReservationStation.module.io.loadEarlyWakeup := memoryReservationStation.module.io.loadEarlyWakeup
    floatingReservationStation.module.io.earlyWakeUpCancel := memoryBlock.module.io.earlyWakeUpCancel

    memoryReservationStation.module.io.redirect := Pipe(localRedirect)
    memoryReservationStation.module.io.enq <> io.enqMem
    memoryReservationStation.module.io.specWakeup := integerReservationStation.module.io.specWakeup
    memoryReservationStation.module.io.earlyWakeUpCancel := memoryBlock.module.io.earlyWakeUpCancel

    private val pcMem = Module(new PcMem(pcMemEntries, regFile.module.pcReadNum, 1))
    pcMem.io.write.head := io.pcMemWrite

    pcMem.io.read.zip(regFile.module.io.pcReadAddr ++ writebackNetwork.module.io.pcReadAddr).foreach({ case (r, addr) => r.addr := addr })
    (regFile.module.io.pcReadData ++ writebackNetwork.module.io.pcReadData).zip(pcMem.io.read).foreach({ case (data, r) => data := r.data })

    integerBlock.module.io.fenceio <> io.fenceio
    memoryBlock.module.io.issueToMou <> integerBlock.module.io.issueToMou
    memoryBlock.module.io.writebackFromMou <> integerBlock.module.io.writebackFromMou

    memoryBlock.module.redirectIn := Pipe(localRedirect)
    integerBlock.module.redirectIn := Pipe(localRedirect)
    floatingBlock.module.redirectIn := Pipe(localRedirect)

    io.redirectOut := writebackNetwork.module.io.redirectOut
  }
}
