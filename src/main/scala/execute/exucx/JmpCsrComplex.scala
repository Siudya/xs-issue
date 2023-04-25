package execute.exucx
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.Valid
import common.{ExuOutput}
import exu.{ExuType, FenceIO, JmpCsrExu}
import freechips.rocketchip.diplomacy.LazyModule
import xs.utils.Assertion.xs_assert

class JmpCsrComplex(id: Int, bypassNum:Int)(implicit p:Parameters) extends BasicExuComplex{
  val jmp = LazyModule(new JmpCsrExu(id, "JmpCsrComplex", bypassNum))
  jmp.issueNode :*= issueNode
  writebackNode :=* jmp.writebackNode
  lazy val module = new BasicExuComplexImp(this){
    val io = IO(new Bundle {
      val bypassIn = Input(Vec(bypassNum, Valid(new ExuOutput)))
      val fenceio = new FenceIO
    })
    private val issueIn = issueNode.in.head._1
    private val issueJmp = issueNode.out.filter(_._2.exuType == ExuType.jmp).head._1
    issueJmp <> issueIn
    jmp.module.io.bypassIn := io.bypassIn
    jmp.module.redirectIn := redirectIn

    jmp.module.io.fenceio <> io.fenceio
  }
}
