package exu
import chipsalliance.rocketchip.config
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util._
import common.{ExuInput, ExuOutput}
import freechips.rocketchip.diplomacy._
import issue.{IssueBundle, RsParam}
object ExuInwardImpl extends SimpleNodeImp[Option[RsParam],ExuConfig,ExuConfig,IssueBundle]{
  override def edge(pd: Option[RsParam], pu: ExuConfig, p: config.Parameters, sourceInfo: SourceInfo) = pu
  override def bundle(e: ExuConfig): IssueBundle = new IssueBundle(e.releaseWidth)
  override def render(e: ExuConfig) = RenderedEdge("#00ff00", e.name)
}
object ExuOutwardImpl extends SimpleNodeImp[ExuConfig, Option[ExuConfig],ExuConfig,Valid[ExuOutput]]{
  override def edge(pd: ExuConfig, pu: Option[ExuConfig], p: config.Parameters, sourceInfo: SourceInfo) = pd
  override def bundle(eo: ExuConfig): Valid[ExuOutput] = Valid(new ExuOutput)
  override def render(e: ExuConfig) = RenderedEdge("#0000ff", e.name)
}

class ExuInputNode(exuConfig: ExuConfig)(implicit valName: ValName) extends SinkNode(ExuInwardImpl)(Seq(exuConfig))
class ExuOutNode(exuConfig: ExuConfig)(implicit valName: ValName) extends SourceNode(ExuOutwardImpl)(Seq(exuConfig))