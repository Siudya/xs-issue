package exu
import chipsalliance.rocketchip.config
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util._
import common.{ExuInput, ExuOutput}
import exu.ExuConfig
import freechips.rocketchip.diplomacy._
object ExuInwardImpl extends SimpleNodeImp[Option[ExuConfig],ExuConfig,ExuConfig,Valid[ExuInput]]{
  override def edge(pd: Option[ExuConfig], pu: ExuConfig, p: config.Parameters, sourceInfo: SourceInfo) = pu
  override def bundle(e: ExuConfig) = ValidIO(new ExuInput(e.srcNum))
  override def render(e: ExuConfig) = RenderedEdge("#00ff00", e.name)
}
object ExuOutwardImpl extends SimpleNodeImp[ExuConfig, Option[ExuConfig],ExuConfig,Valid[ExuOutput]]{
  override def edge(pd: ExuConfig, pu: Option[ExuConfig], p: config.Parameters, sourceInfo: SourceInfo) = pd
  override def bundle(eo: ExuConfig): Valid[ExuOutput] = Valid(new ExuOutput())
  override def render(e: ExuConfig) = RenderedEdge("#0000ff", e.name)
}

class ExuInputNode(exuConfig: ExuConfig) extends SinkNode(ExuInwardImpl)(Seq(exuConfig))
class ExuOutNode(exuConfig: ExuConfig) extends SourceNode(ExuOutwardImpl)(Seq(exuConfig))