import chisel3.stage.ChiselGeneratorAnnotation
import exu.{AluExu, DivExu, ExuConfig, MulExu}
import freechips.rocketchip.diplomacy._
import xs.utils.Assertion
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import common.{ExuInput, ExuOutput, Redirect}
import regfile.RegfileWriteBackImpl
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util._
case object ParamKey extends Field[Param]
case class Param
(
  XLEN:Int = 64
)
class MyConfig extends Config((site, here, up) => {
  case ParamKey => Param()
})

object issueNodeImpl extends SimpleNodeImp[Option[ExuConfig],ExuConfig, ExuConfig, ExuInput]{
  override def edge(pd: Option[ExuConfig], pu: ExuConfig, p: Parameters, sourceInfo: SourceInfo): ExuConfig = pu
  override def bundle(e: ExuConfig): ExuInput = new ExuInput(e.releaseWidth)
  override def render(e: ExuConfig): RenderedEdge = RenderedEdge("#0000ff", e.name)
}

class WriteBackNode(paramSeq:Seq[Option[ExuConfig]])(implicit valName: ValName) extends
  SinkNode(RegfileWriteBackImpl)(paramSeq)

class IssueNode(paramSeq:Seq[Option[ExuConfig]])(implicit valName: ValName) extends
  SourceNode(issueNodeImpl)(paramSeq)

class FakeTop(implicit p:Parameters) extends LazyModule{
  val issueNode = new IssueNode(Seq(None))
  val outNode = new WriteBackNode(Seq(None))
  val inst = LazyModule(new DivExu(1, 2))
  inst.issueNode :*= issueNode
  outNode :=* inst.writebackNode
  lazy val module = new LazyModuleImp(this){
    issueNode.makeIOs()
    outNode.makeIOs()
    val io = IO(new Bundle {
      val redirectIn = Input(Valid(new Redirect))
//      val redirectOut = Output(Valid(new Redirect))
      val bypassIn = Input(Vec(2, Valid(new ExuOutput)))
//      val bypassOut = Output(Valid(new ExuOutput))
    })
    inst.module.io.redirectIn := io.redirectIn
    inst.module.io.bypassIn := io.bypassIn
//    io.redirectOut := inst.module.io.redirectOut
//    io.bypassOut := inst.module.io.bypassOut

    println(issueNode.out.head._2)
  }
}

object GenRtl extends App {
  Assertion.set_enable(false)
  implicit val p: MyConfig = new MyConfig
  val top = LazyModule(new FakeTop)
  (new chisel3.stage.ChiselStage).execute(args,
    Seq(
      ChiselGeneratorAnnotation(() => top.module)
    )
  )
}