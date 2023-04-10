package regfile
import chipsalliance.rocketchip.config
import freechips.rocketchip.diplomacy._
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util._
import issue.{ExuConfig, ExuInput, ExuOutput, MicroOp}

object RegfileInwardImpl extends InwardNodeImp[ExuConfig,ExuConfig,ExuConfig,MicroOp]{
  override def edgeI(pd: ExuConfig, pu: ExuConfig, p: config.Parameters, sourceInfo: SourceInfo) = pu
  override def bundleI(ei: ExuConfig) = new MicroOp
  override def render(e: ExuConfig) = RenderedEdge("green", e.name)
}

object RegfileOutwardImpl extends OutwardNodeImp[ExuConfig,ExuConfig,ExuConfig,ExuInput]{
  override def edgeO(pd: ExuConfig, pu: ExuConfig, p: config.Parameters, sourceInfo: SourceInfo) = pu
  override def bundleO(eo: ExuConfig) = new ExuInput(eo.srcNum)
}

object RegfileWriteBackImpl extends SimpleNodeImp[ExuConfig,ExuConfig,ExuConfig, ExuOutput]{
  override def edge(pd: ExuConfig, pu: ExuConfig, p: config.Parameters, sourceInfo: SourceInfo) = pd
  override def bundle(e: ExuConfig) = new ExuOutput
  override def render(e: ExuConfig) = RenderedEdge("blue", e.name)
}
object RegfileNodeUtils{
  def dFn(di:ExuConfig):ExuConfig = di
  def uFn(ui:ExuConfig):ExuConfig = ui
}

class RegfileIssueNode extends
  MixedAdapterNode(inner = RegfileInwardImpl, outer = RegfileOutwardImpl)(RegfileNodeUtils.dFn, RegfileNodeUtils.uFn){
}
class RegfileWriteBackNode extends
  SinkNode(RegfileWriteBackImpl){
}