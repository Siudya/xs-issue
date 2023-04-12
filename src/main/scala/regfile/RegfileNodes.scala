package regfile
import chipsalliance.rocketchip.config
import freechips.rocketchip.diplomacy._
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util._
import common.{ExuInput, ExuOutput, MicroOp}
import exu.ExuConfig
import issue.IssueBundle

object RegfileInwardImpl extends InwardNodeImp[Option[ExuConfig],ExuConfig,ExuConfig,IssueBundle]{
  override def edgeI(pd: Option[ExuConfig], pu: ExuConfig, p: config.Parameters, sourceInfo: SourceInfo) = pu
  override def bundleI(ei: ExuConfig) = new IssueBundle(ei.releaseWidth)
  override def render(e: ExuConfig) = RenderedEdge("#00ff00", e.name)
}
object RegfileOutwardImpl extends OutwardNodeImp[Option[ExuConfig],ExuConfig,ExuConfig,ExuInput]{
  override def edgeO(pd: Option[ExuConfig], pu: ExuConfig, p: config.Parameters, sourceInfo: SourceInfo) = pu
  override def bundleO(eo: ExuConfig): ExuInput = new ExuInput(eo.releaseWidth)
}
object RegfileWriteBackImpl extends SimpleNodeImp[ExuConfig,Option[ExuConfig],ExuConfig, Valid[ExuOutput]]{
  override def edge(pd: ExuConfig, pu: Option[ExuConfig], p: config.Parameters, sourceInfo: SourceInfo) = pd
  override def bundle(e: ExuConfig): Valid[ExuOutput] = Valid(new ExuOutput)
  override def render(e: ExuConfig) = RenderedEdge("#0000ff", e.name)
}
class RegfileIssueNode(implicit valName: ValName)
  extends MixedAdapterNode(
    inner = RegfileInwardImpl, outer = RegfileOutwardImpl
  )(
    {p:Option[ExuConfig] =>p}, {p:ExuConfig =>p}
  )

class RegfileWriteBackNode(paramSeq:Seq[Option[ExuConfig]])(implicit valName: ValName) extends
  SinkNode(RegfileWriteBackImpl)(paramSeq){
}