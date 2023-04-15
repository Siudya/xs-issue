package issue
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config
import freechips.rocketchip.diplomacy._
import chisel3.internal.sourceinfo.SourceInfo
import common.{ExuOutput, MicroOp}
import exu.ExuConfig


object RsIssueNodeImpl extends SimpleNodeImp[RsParam, Seq[ExuConfig], Seq[ExuConfig], Vec[IssueBundle]]{
  override def edge(pd: RsParam, pu: Seq[ExuConfig], p: config.Parameters, sourceInfo: SourceInfo): Seq[ExuConfig] = pu
  override def bundle(e: Seq[ExuConfig]): Vec[IssueBundle] = Vec(e.length, new IssueBundle)
  override def render(e: Seq[ExuConfig]): RenderedEdge = RenderedEdge("#00ff00", "Integer Issue")
}
object RsDispatchNodeImpl extends SimpleNodeImp[Option[DispatchParam], DispatchParam, DispatchParam, Vec[DecoupledIO[MicroOp]]] {
  override def edge(pd: Option[DispatchParam], pu: DispatchParam, p: config.Parameters, sourceInfo: SourceInfo): DispatchParam = pu
  override def bundle(e: DispatchParam): Vec[DecoupledIO[MicroOp]] = Vec(e.width, DecoupledIO(new MicroOp))
  override def render(e: DispatchParam): RenderedEdge = RenderedEdge("#ff0000", e.name)
}
class RsIssueNode(param:RsParam)(implicit valName: ValName) extends SourceNode(RsIssueNodeImpl)(Seq(param))
class RsDispatchNode(paramSeq:Seq[DispatchParam])(implicit valName: ValName) extends SinkNode(RsDispatchNodeImpl)(paramSeq)