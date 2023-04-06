package issue.IntRs
import chisel3._
import chisel3.util._
import issue._
class IntegerReservation(param:RsParam) extends XSModule{
  require(param.bankNum == 4)
  param.issuePortFuTypes.foreach(elm => require(param.bankNum % elm._1 == 0))
  param.issuePortFuTypes.foreach(elm => require(elm._1 >= 1))
  require(param.entriesNum % param.bankNum == 0)
  private val issueTypeNum = param.issuePortFuTypes.length
  private val issueNumForEachFu = param.issuePortFuTypes.map(_._1)
  private val internalWakeupNum = param.issuePortFuTypes.filterNot(_._2.contains(FuType.jmp)).map(_._1).sum
  private val entriesNumPerBank = param.entriesNum / param.bankNum
  private val issuePortNum = param.issuePortFuTypes.map(_._1).sum
  private val specWakupPortNum = param.issuePortFuTypes.filter(_._2.contains(FuType.alu)).map(_._1).sum
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val enq = Vec(param.bankNum, Flipped(DecoupledIO(new MicroOp)))
    val issue = MixedVec(issueNumForEachFu.map(num => Vec(num, DecoupledIO(new MicroOp))))
    val wakeup = Input(Vec(param.wakeUpPortNum, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val specWakeup = Output(Vec(specWakupPortNum, Valid(new EarlyWakeUpInfo)))
  })

  private val internalWakeup = Wire(Vec(internalWakeupNum, Valid(new WakeUpInfo)))
  //TODO: remove this!
  internalWakeup := DontCare

  private val rsBankSeq = Seq.tabulate(param.bankNum)( _ => {
    val mod = Module(new IntegerReservationStationBank(entriesNumPerBank, issueTypeNum, internalWakeupNum + param.wakeUpPortNum, loadUnitNum))
    mod.io.redirect := io.redirect
    mod.io.wakeup := io.wakeup ++ internalWakeup
    mod.io.loadEarlyWakeup := io.loadEarlyWakeup
    mod.io.earlyWakeUpCancel := io.earlyWakeUpCancel
    mod
  })
  private val allocateNetwork = Module(new AllocateNetwork(param.bankNum, entriesNumPerBank))
  private val selectNetworkSeq = param.issuePortFuTypes.map(elm => {
    val mod = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, elm._1, elm._2))
    mod.io.redirect := io.redirect
    mod
  })

  allocateNetwork.io.enqFromDispatch.zip(io.enq).foreach({case(sink, source) =>
    sink.valid := source.valid
    sink.bits := source.bits
    source.ready := sink.ready
  })

  for(((fromAllocate, toAllocate), rsBank) <- allocateNetwork.io.enqToRs
    .zip(allocateNetwork.io.entriesValidBitVecList)
    .zip(rsBankSeq)){
    toAllocate := rsBank.io.allocateInfo
    rsBank.io.enq.valid := fromAllocate.valid
    rsBank.io.enq.bits.data := fromAllocate.bits.uop
    rsBank.io.enq.bits.addrOH := fromAllocate.bits.addrOH
  }

  selectNetworkSeq.foreach(sn => {
    sn.io.selectInfo.zip(rsBankSeq).foreach({case(sink, source) =>
      sink := source.io.selectInfo
    })
  })

  private val issRegs = Seq.tabulate(issueTypeNum)(idx =>
    RegInit(VecInit(Seq.tabulate(issueNumForEachFu(idx))(_ => 0.U.asTypeOf(Valid(new MicroOp)))))
  )

  for((((iss, issR), sn), fuIdx) <- io.issue.zip(issRegs).zip(selectNetworkSeq).zipWithIndex){
    for(((iss_elm, issR_elm), portIdx) <- iss.zip(issR).zipWithIndex){
      val issueBundle = Wire(Valid(new MicroOp))
      val rbIssAddrPorts = rsBankSeq.map(_.io.issueAddr(fuIdx))
      val rbIssDataPorts = rsBankSeq.map(_.io.issueData(fuIdx))
      val bn = param.bankNum / iss.length
      val rbAddrPortsForThisPort = rbIssAddrPorts.slice(portIdx * bn, portIdx * bn + bn)
      val rbDataPortsForThisPort = rbIssDataPorts.slice(portIdx * bn, portIdx * bn + bn)
      val selectResps = sn.io.issueInfo(portIdx)
      issueBundle.valid := Cat(rbDataPortsForThisPort.map(_.valid)).orR
      issueBundle.bits := Mux1H(rbDataPortsForThisPort.map(elm => (elm.valid, elm.bits)))
      issueBundle.bits.robIdx := selectResps.bits.info.robPtr
      issueBundle.bits.ctrl.rfWen := selectResps.bits.info.rfWen
      issueBundle.bits.ctrl.rfWen := selectResps.bits.info.fpWen
      issueBundle.bits.pdest := selectResps.bits.info.pdest
      issueBundle.bits.ctrl.fuType := selectResps.bits.info.fuType
      val successSelect = !issR_elm.valid || iss_elm.fire
      iss_elm.valid := issR_elm.valid
      iss_elm.bits := issR_elm.bits

      when(successSelect){
        issR_elm.valid := issueBundle.valid
      }
      when(successSelect && issueBundle.valid){
        issR_elm.bits := issueBundle.bits
      }

      rbAddrPortsForThisPort.zipWithIndex.foreach({case(rb,bidx) =>
        rb.valid := sn.io.issueInfo.head.valid & sn.io.issueInfo.head.bits.bankIdxOH(bidx + portIdx * bn) & successSelect
        rb.bits := sn.io.issueInfo.head.bits.entryIdxOH
      })
    }
  }
  io.specWakeup := DontCare
}
