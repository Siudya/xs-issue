package execute.exucx
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import common.{ExuOutput, FuType, Redirect}
import exu.{AluExu, ExuType, FenceIO, JmpCsrExu}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xs.utils.Assertion.xs_assert

class AluJmpComplex(id: Int, bypassNum:Int)(implicit p:Parameters) extends LazyModule{
  val issueNode = new ExuComplexIssueNode
  val writebackNode = new ExuComplexWritebackNode
  val alu = new AluExu(id, bypassNum)
  val jmp = new JmpCsrExu(id, bypassNum)
  alu.issueNode :*= issueNode
  jmp.issueNode :*= issueNode
  writebackNode :=* alu.writebackNode
  writebackNode :=* jmp.writebackNode
  lazy val module = new LazyModuleImp(this){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 2)
    val io = IO(new Bundle{
      val redirect = Input(Valid(new Redirect))
      val bypassIn = Input(Vec(bypassNum, Valid(new ExuOutput)))
      val fenceio = new FenceIO
    })
    private val issueIn = issueNode.in.head._1
    private val issueAlu = issueNode.out.filter(_._2.exuType == ExuType.alu).head._1
    private val issueJmp = issueNode.out.filter(_._2.exuType == ExuType.jmp).head._1

    issueAlu <> issueIn
    alu.module.io.bypassIn := io.bypassIn
    alu.module.redirectIn := io.redirect

    issueJmp <> issueIn
    jmp.module.io.bypassIn := io.bypassIn
    jmp.module.redirectIn := io.redirect

    jmp.module.io.fenceio <> io.fenceio

    issueIn.fuInFire := DontCare
    issueIn.issue.ready := Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.alu, issueAlu.issue.ready, issueJmp.issue.ready)
    private val issueFuHit = issueNode.in.head._2.exuConfigs.flatMap(_.fuConfigs).map(_.fuType === issueIn.issue.bits.uop.ctrl.fuType).reduce(_ | _)
    xs_assert(Mux(issueIn.issue.valid, issueFuHit, true.B))
  }
}
