package regfile
import chipsalliance.rocketchip.config
import freechips.rocketchip.diplomacy._
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util._
import issue.{ExuConfig, ExuInput, ExuOutput, MicroOp}

object RegfileInwardImpl extends InwardNodeImp[Option[ExuConfig],ExuConfig,ExuConfig,Valid[MicroOp]]{
  override def edgeI(pd: Option[ExuConfig], pu: ExuConfig, p: config.Parameters, sourceInfo: SourceInfo) = pu
  override def bundleI(ei: ExuConfig) = Valid(new MicroOp)
  override def render(e: ExuConfig) = RenderedEdge("#00ff00", e.name)
}
object RegfileOutwardImpl extends OutwardNodeImp[Option[ExuConfig],ExuConfig,ExuConfig,Valid[ExuInput]]{
  override def edgeO(pd: Option[ExuConfig], pu: ExuConfig, p: config.Parameters, sourceInfo: SourceInfo) = pu
  override def bundleO(eo: ExuConfig) = Valid(new ExuInput(eo.srcNum))
}
object RegfileWriteBackImpl extends SimpleNodeImp[ExuConfig,Option[ExuConfig],ExuConfig, Valid[ExuOutput]]{
  override def edge(pd: ExuConfig, pu: Option[ExuConfig], p: config.Parameters, sourceInfo: SourceInfo) = pd
  override def bundle(e: ExuConfig) = Valid(new ExuOutput)
  override def render(e: ExuConfig) = RenderedEdge("#0000ff", e.name)
}
class RegfileIssueNode extends MixedAdapterNode(inner = RegfileInwardImpl, outer = RegfileOutwardImpl)({p:Option[ExuConfig] =>p}, {p:ExuConfig =>p})

class RegfileWriteBackNode(paramSeq:Seq[Option[ExuConfig]]) extends
  SinkNode(RegfileWriteBackImpl)(paramSeq){
}