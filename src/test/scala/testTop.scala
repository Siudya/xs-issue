import chisel3.stage.ChiselGeneratorAnnotation
import exu.{AluExu, DivExu, ExuConfig, MulExu}
import freechips.rocketchip.diplomacy._
import xs.utils.Assertion
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import common.{ExuInput, ExuOutput, Redirect}
import regfile.{Regfile, RegfileWriteBackImpl}
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util._
import issue.RsIssueNode
case object ParamKey extends Field[Param]
case class Param
(
  XLEN:Int = 64
)
class MyConfig extends Config((site, here, up) => {
  case ParamKey => Param()
})

class FakeTop(implicit p:Parameters) extends LazyModule{
  val issueNode = new RsIssueNode(Seq.fill(7)(None))
  private val regFile = LazyModule(new Regfile(7, 32, "Int"))
  private val alus = Seq.tabulate(4)(idx => LazyModule(new AluExu(idx, 0)))
  private val muls = Seq.tabulate(2)(idx => LazyModule(new MulExu(idx, 0)))
  private val divs = Seq.tabulate(1)(idx => LazyModule(new DivExu(idx, 0)))
  private val exus = alus ++ muls ++ divs

  regFile.issueNode :*= issueNode
  for(exu <- exus){
    exu.issueNode :*= regFile.issueNode
    regFile.writeBackNode :=* exu.writebackNode
  }

  lazy val module = new LazyModuleImp(this){
    issueNode.makeIOs()
    val io = IO(new Bundle {
      val redirectIn = Input(Valid(new Redirect))
//      val redirectOut = Output(Valid(new Redirect))
      val bypassIn = Input(Vec(2, Valid(new ExuOutput)))
//      val bypassOut = Output(Valid(new ExuOutput))
    })
    exus.foreach(_.module.redirectIn := io.redirectIn)
    regFile.module.io.redirect := io.redirectIn

    issueNode.out.foreach(edge => println(edge._2))
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