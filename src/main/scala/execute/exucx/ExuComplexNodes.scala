package execute.exucx
import chipsalliance.rocketchip.config
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util._
import common.{ExuInput, ExuOutput}
import exu.{ExuConfig, ExuOutwardImpl, ExuType}
import freechips.rocketchip.diplomacy._
import issue.{IssueBundle, RsParam}

case class ExuComplexParam
(
  val id:Int,
  val exuConfigs: Seq[ExuConfig]
){
  val name: String = exuConfigs.head.complexName
  val hasJmp: Boolean = exuConfigs.map(_.exuType == ExuType.jmp).reduce(_ || _)
  val hasAlu: Boolean = exuConfigs.map(_.exuType == ExuType.alu).reduce(_ || _)
  val hasMul: Boolean = exuConfigs.map(_.exuType == ExuType.mul).reduce(_ || _)
  val hasDiv: Boolean = exuConfigs.map(_.exuType == ExuType.div).reduce(_ || _)
  val hasFmac: Boolean = exuConfigs.map(_.exuType == ExuType.fmac).reduce(_ || _)
  val hasFmisc: Boolean = exuConfigs.map(_.exuType == ExuType.fmisc).reduce(_ || _)
  val hasFdiv: Boolean = exuConfigs.map(_.exuType == ExuType.fdiv).reduce(_ || _)
  val hasLoad: Boolean = exuConfigs.map(_.exuType == ExuType.load).reduce(_ || _)
  val hasSta: Boolean = exuConfigs.map(_.exuType == ExuType.sta).reduce(_ || _)
  val hasStd: Boolean = exuConfigs.map(_.exuType == ExuType.std).reduce(_ || _)
  val isIntType:Boolean = exuConfigs.head.isIntType
  val isFpType:Boolean = exuConfigs.head.isFpType
  val isMemType:Boolean = exuConfigs.head.isMemType
  val srcNum:Int = exuConfigs.map(_.srcNum).max

  val isAluDiv:Boolean = hasDiv && hasAlu
  val isAluJmp:Boolean = hasJmp && hasAlu
  val isAluMul:Boolean = hasMul && hasAlu
  val isFmac:Boolean = hasFmac && !hasFdiv && !hasFmisc
  val isFmaDiv:Boolean = hasFmac && hasFdiv
  val isFmaMisc:Boolean = hasFmac && hasFmisc

  override def toString:String = s"${name}: " + exuConfigs.map(_.toString).reduce(_++_)
}
object ExuComplexIssueInwardNodeImpl extends InwardNodeImp[Option[RsParam], ExuComplexParam, ExuComplexParam, IssueBundle]{
  override def edgeI(pd: Option[RsParam], pu: ExuComplexParam, p: config.Parameters, sourceInfo: SourceInfo): ExuComplexParam = pu
  override def bundleI(ei: ExuComplexParam): IssueBundle = new IssueBundle
  override def render(ei: ExuComplexParam): RenderedEdge = RenderedEdge("#0000ff", ei.exuConfigs.map(_.name + "_").reduce(_++_))
}
object ExuComplexIssueOutwardNodeImpl extends OutwardNodeImp[Option[RsParam], ExuConfig, ExuConfig, IssueBundle]{
  override def edgeO(pd: Option[RsParam], pu: ExuConfig, p: config.Parameters, sourceInfo: SourceInfo): ExuConfig = pu
  override def bundleO(eo: ExuConfig): IssueBundle = new IssueBundle
}
class ExuComplexIssueNode(implicit valName: ValName) extends
  MixedNexusNode(inner = ExuComplexIssueInwardNodeImpl, outer = ExuComplexIssueOutwardNodeImpl)(
    dFn = {p:Seq[Option[RsParam]] => None},
    uFn = {p:Seq[ExuConfig] => ExuComplexParam(p.head.id, p)}
  )

class ExuComplexWritebackNode(implicit valName: ValName) extends
  AdapterNode(ExuOutwardImpl)({p => p}, {p => p})