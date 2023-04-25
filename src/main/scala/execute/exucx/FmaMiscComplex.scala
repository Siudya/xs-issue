package execute.exucx

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import common.{FuType, Redirect}
import execute.exu.{ExuType, FmacExu, FmiscExu}
import freechips.rocketchip.diplomacy.LazyModule
import xs.utils.Assertion.xs_assert

class FmaMiscComplex(id: Int)(implicit p:Parameters) extends BasicExuComplex{
  val fmisc = LazyModule(new FmiscExu(id, "FmaMiscComplex"))
  val fmac = LazyModule(new FmacExu(id, "FmaMiscComplex"))
  fmisc.issueNode :*= issueNode
  fmac.issueNode :*= issueNode
  writebackNode :=* fmisc.writebackNode
  writebackNode :=* fmac.writebackNode
  lazy val module = new BasicExuComplexImp(this){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 2)
    private val issueIn = issueNode.in.head._1
    private val issueFmac = issueNode.out.filter(_._2.exuType == ExuType.fmac).head._1
    private val issueFmisc = issueNode.out.filter(_._2.exuType == ExuType.fmisc).head._1

    issueFmac <> issueIn
    fmac.module.redirectIn := redirectIn

    issueFmisc <> issueIn
    fmisc.module.redirectIn := redirectIn

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
