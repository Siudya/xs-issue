package issue
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config
import freechips.rocketchip.diplomacy._
import chisel3.internal.sourceinfo.SourceInfo

object RsIssueNodeImpl extends SimpleNodeImp[Option[ExuConfig], ExuConfig, ExuConfig, Valid[MicroOp]]{
  override def edge(pd: Option[ExuConfig], pu: ExuConfig, p: config.Parameters, sourceInfo: SourceInfo): ExuConfig = pu
  override def bundle(e: ExuConfig): Valid[MicroOp] = Valid(new MicroOp)
  override def render(e: ExuConfig): RenderedEdge = RenderedEdge("#00ff00", e.name)
}
object RsDispatchNodeImpl extends SimpleNodeImp[Option[DispatchParam], DispatchParam, DispatchParam, Vec[Valid[MicroOp]]] {
  override def edge(pd: Option[DispatchParam], pu: DispatchParam, p: config.Parameters, sourceInfo: SourceInfo): DispatchParam = pu
  override def bundle(e: DispatchParam): Vec[Valid[MicroOp]] = Vec(e.width, Valid(new MicroOp))
  override def render(e: DispatchParam): RenderedEdge = RenderedEdge("#ff0000", e.name)
}
class RsIssueNode(paramSeq:Seq[Option[ExuConfig]]) extends SourceNode(RsIssueNodeImpl)(paramSeq)
class RsDispatchNode(paramSeq:Seq[DispatchParam]) extends SinkNode(RsDispatchNodeImpl)(paramSeq)