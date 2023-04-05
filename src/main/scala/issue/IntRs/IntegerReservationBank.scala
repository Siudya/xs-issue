package issue.IntRs
import issue._
import chisel3._
import chisel3.util._

class IntegerPayloadEntryEncoder extends Module{
  val io = IO(new Bundle{
    val in = Input(new MicroOp)
    val out = Output(new MicroOp)
  })
  io.out := io.in
}

class IntegerPayloadEntryDecoder extends Module{
  val io = IO(new Bundle{
    val in = Input(new MicroOp)
    val out = Output(new MicroOp)
  })
  io.out := io.in
}

class MicroOpToIntegerStatusArrayEntry extends Module{
  val io = IO(new Bundle{
    val in = Input(new MicroOp)
    val out = Output(new IntegerStatusArrayEntry)
  })
  io.out.psrc(0) := io.in.psrc(0)
  io.out.psrc(1) := io.in.psrc(1)
  io.out.srcType(0) := io.in.ctrl.srcType(0)
  io.out.srcType(1) := io.in.ctrl.srcType(1)
  io.out.srcState(0) := io.in.srcState(0)
  io.out.srcState(1) := io.in.srcState(1)
  io.out.pdest := io.in.pdest
  io.out.lpv.foreach(_.foreach(_ := 0.U))
  io.out.fuType := io.in.ctrl.fuType
  io.out.rfWen := io.in.ctrl.rfWen
  io.out.fpWen := io.in.ctrl.fpWen
  io.out.robIdx := io.in.robIdx
  io.out.issued := false.B
}
class IntegerReservationStationBank(entryNum:Int, issueWidth:Int, wakeupWidth:Int, loadUnitNum:Int) extends Module{
  val io = IO(new Bundle {
    val redirect = Input(Valid(new Redirect))

    val issueInfo = Output(Vec(entryNum, Valid(new SelectInfo)))
    val allocateInfo = Output(UInt(entryNum.W))

    val enq = Input(Valid(new Bundle {
      val addrOH = UInt(entryNum.W)
      val data = new MicroOp
    }))

    val issueAddr = Input(Vec(issueWidth, Valid(UInt(entryNum.W))))
    val issueData = Output(Vec(issueWidth, Valid(new MicroOp)))
    val wakeup = Input(Vec(wakeupWidth, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
  })

  private val enqToStatusCvt = Module(new MicroOpToIntegerStatusArrayEntry)
  private val statusArray = Module(new IntegerStatusArray(entryNum, issueWidth, wakeupWidth, loadUnitNum))
  private val enqToPayloadCvt = Module(new IntegerPayloadEntryEncoder)
  private val payloadToDeqCvts = io.issueAddr.indices.map(i => Module(new IntegerPayloadEntryDecoder))
  private val payloadArray = Module(new PayloadArray(entryNum, issueWidth, "IntegerPayloadArray"))

  enqToStatusCvt.io.in := io.enq.bits.data
  statusArray.io.redirect := io.redirect
  io.issueInfo := statusArray.io.issueInfo
  io.allocateInfo := statusArray.io.allocateInfo
  statusArray.io.enq.valid := io.enq.valid
  statusArray.io.enq.bits.addrOH := io.enq.bits.addrOH
  statusArray.io.enq.bits.data := enqToStatusCvt.io.out
  statusArray.io.issue := io.issueAddr
  statusArray.io.wakeup := io.wakeup
  statusArray.io.loadEarlyWakeup := io.loadEarlyWakeup
  statusArray.io.earlyWakeUpCancel := io.earlyWakeUpCancel

  payloadArray.io.write.en := io.enq.valid
  payloadArray.io.write.addr := io.enq.bits.addrOH
  enqToPayloadCvt.io.in := io.enq.bits.data
  payloadArray.io.write.data := enqToPayloadCvt.io.out
  payloadArray.io.read.zip(io.issueAddr).zip(io.issueData).zip(payloadToDeqCvts).foreach({
    case(((port, iAddr), iData), iCvt) =>{
      port.addr := iAddr.bits
      iCvt.io.in := port.data
      iData.bits := iCvt.io.out
      iData.valid := iAddr.valid
    }
  })
}
