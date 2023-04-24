package execute.exucx

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import common.{FuType, Redirect}
import exu.{ExuType, FmacExu, FmiscExu}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xs.utils.Assertion.xs_assert

class FmaMiscComplex(id: Int)(implicit p:Parameters) extends LazyModule{
  val issueNode = new ExuComplexIssueNode
  val writebackNode = new ExuComplexWritebackNode
  val fmisc = new FmiscExu(id, "FmaMiscComplex")
  val fmac = new FmacExu(id, "FmaMiscComplex")
  fmisc.issueNode :*= issueNode
  fmac.issueNode :*= issueNode
  writebackNode :=* fmisc.writebackNode
  writebackNode :=* fmac.writebackNode
  lazy val module = new LazyModuleImp(this){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 2)
    val io = IO(new Bundle{
      val redirect = Input(Valid(new Redirect))
    })
    private val issueIn = issueNode.in.head._1
    private val issueFmac = issueNode.out.filter(_._2.exuType == ExuType.fmac).head._1
    private val issueFmisc = issueNode.out.filter(_._2.exuType == ExuType.fmisc).head._1

    issueFmac <> issueIn
    fmac.module.redirectIn := io.redirect

    issueFmisc <> issueIn
    fmisc.module.redirectIn := io.redirect

    issueIn.fmaMidState <> issueFmac.fmaMidState
    issueFmisc.fmaMidState.in.valid := false.B
    issueFmisc.fmaMidState.in.bits := DontCare
    issueFmisc.fmaMidState.waitForAdd := false.B

    issueIn.fuInFire := DontCare
    issueIn.issue.ready := Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.fmac, issueFmac.issue.ready, issueFmisc.issue.ready)
    private val issueFuHit = issueNode.in.head._2.exuConfigs.flatMap(_.fuConfigs).map(_.fuType === issueIn.issue.bits.uop.ctrl.fuType).reduce(_ | _)
    xs_assert(Mux(issueIn.issue.valid, issueFuHit, true.B))
  }
}
