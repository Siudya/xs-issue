package execute.exucx
import chipsalliance.rocketchip.config
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util._
import common.{ExuInput, ExuOutput}
import exu.{ExuConfig, ExuOutwardImpl}
import freechips.rocketchip.diplomacy._
import issue.{IssueBundle, RsParam}
object ExuComplexIssueInwardNodeImpl extends InwardNodeImp[Option[RsParam], Seq[ExuConfig], Seq[ExuConfig], IssueBundle]{
  override def edgeI(pd: Option[RsParam], pu: Seq[ExuConfig], p: config.Parameters, sourceInfo: SourceInfo): Seq[ExuConfig] = pu
  override def bundleI(ei: Seq[ExuConfig]): IssueBundle = new IssueBundle
  override def render(ei: Seq[ExuConfig]): RenderedEdge = RenderedEdge("#0000ff", ei.map(_.name + "_").reduce(_++_))
}
object ExuComplexIssueOutwardNodeImpl extends OutwardNodeImp[Option[RsParam], ExuConfig, ExuConfig, IssueBundle]{
  override def edgeO(pd: Option[RsParam], pu: ExuConfig, p: config.Parameters, sourceInfo: SourceInfo): ExuConfig = pu
  override def bundleO(eo: ExuConfig): IssueBundle = new IssueBundle
}
class ExuComplexIssueNode(implicit valName: ValName) extends
  MixedNexusNode(inner = ExuComplexIssueInwardNodeImpl, outer = ExuComplexIssueOutwardNodeImpl)(
    dFn = {p:Seq[Option[RsParam]] => None},
    uFn = {p:Seq[ExuConfig] => p}
  )

class ExuComplexWritebackNode(implicit valName: ValName) extends
  AdapterNode(ExuOutwardImpl)({p => p}, {p => p})