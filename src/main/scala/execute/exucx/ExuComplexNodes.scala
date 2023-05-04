package execute.exucx
import chipsalliance.rocketchip.config
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util._
import common.{ExuInput, ExuOutput}
import execute.exu.{ExuConfig, ExuOutwardImpl, ExuType}
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
  val hasI2f: Boolean = exuConfigs.map(_.exuType == ExuType.i2f).reduce(_ || _)
  val hasFmac: Boolean = exuConfigs.map(_.exuType == ExuType.fmac).reduce(_ || _)
  val hasFmisc: Boolean = exuConfigs.map(_.exuType == ExuType.fmisc).reduce(_ || _)
  val hasFdiv: Boolean = exuConfigs.map(_.exuType == ExuType.fdiv).reduce(_ || _)
  val hasLoad: Boolean = exuConfigs.map(_.exuType == ExuType.ldu).reduce(_ || _)
  val hasSta: Boolean = exuConfigs.map(_.exuType == ExuType.sta).reduce(_ || _)
  val hasStd: Boolean = exuConfigs.map(_.exuType == ExuType.std).reduce(_ || _)
  val hasVred:Boolean = exuConfigs.map(_.exuType == ExuType.vred).reduce(_ || _)
  val hasVmisc:Boolean = exuConfigs.map(_.exuType == ExuType.vmisc).reduce(_ || _)
  val hasVfp:Boolean = exuConfigs.map(_.exuType == ExuType.vfp).reduce(_ || _)
  val hasVint:Boolean = exuConfigs.map(_.exuType == ExuType.vint).reduce(_ || _)
  val isIntType:Boolean = exuConfigs.head.isIntType
  val isFpType:Boolean = exuConfigs.head.isFpType
  val isMemType:Boolean = exuConfigs.head.isMemType
  val isVecType:Boolean = exuConfigs.head.isVecType
  val intSrcNum:Int = exuConfigs.map(_.intSrcNum).max
  val fpSrcNum:Int = exuConfigs.map(_.fpSrcNum).max

  val isAluDiv:Boolean = hasAlu && hasDiv
  val isAluI2f:Boolean = hasAlu && hasI2f
  val isAluMul:Boolean = hasAlu && hasMul
  val isJmpCsr:Boolean = hasJmp
  val isFmac:Boolean = hasFmac && !hasFdiv && !hasFmisc
  val isFmaDiv:Boolean = hasFmac && hasFdiv
  val isFmaMisc:Boolean = hasFmac && hasFmisc

  val needToken:Boolean = exuConfigs.map(_.needToken).reduce(_||_)

  val readIntegerRegfile:Boolean = isAluDiv || isAluI2f || isAluMul || isJmpCsr || hasSta || hasStd || hasLoad || hasVmisc || hasVint
  val readFloatingRegfile:Boolean = isFmac || isFmaDiv || isFmaMisc || hasStd || hasVfp
  val readVectorRegfile:Boolean = isVecType || hasLoad || hasStd || hasSta

  override def toString:String = s"${name} #${id} intSrcNum:${intSrcNum} fpSrcNum:${fpSrcNum} " + exuConfigs.map(_.toString).reduce(_++_)
}
object ExuComplexIssueInwardNodeImpl extends InwardNodeImp[Seq[RsParam], ExuComplexParam, (RsParam, ExuComplexParam), IssueBundle]{
  override def edgeI(pd: Seq[RsParam], pu: ExuComplexParam, p: config.Parameters, sourceInfo: SourceInfo): (RsParam, ExuComplexParam) = {
    require(pu.isFpType || pu.isVecType || pu.isIntType || pu.isMemType)
    if (pu.isFpType) {
      (pd.filter(_.isFpRs).head, pu)
    } else if (pu.isVecType) {
      (pd.filter(_.isVecRs).head, pu)
    } else if (pu.isIntType) {
      (pd.filter(_.isIntRs).head, pu)
    } else {
      (pd.filter(_.isMemRs).head, pu)
    }
  }
  override def bundleI(ei: (RsParam, ExuComplexParam)): IssueBundle = new IssueBundle(ei._1.bankNum, ei._1.entriesNum)
  override def render(ei: (RsParam, ExuComplexParam)): RenderedEdge = RenderedEdge("#0000ff", ei._2.exuConfigs.map(_.name).reduce(_++_))
}
object ExuComplexIssueOutwardNodeImpl extends OutwardNodeImp[Seq[RsParam], ExuConfig, (RsParam, ExuConfig), IssueBundle]{
  override def edgeO(pd: Seq[RsParam], pu: ExuConfig, p: config.Parameters, sourceInfo: SourceInfo): (RsParam, ExuConfig) = {
    require(pu.isFpType || pu.isVecType || pu.isIntType || pu.isMemType)
    if (pu.isFpType) {
      (pd.filter(_.isFpRs).head, pu)
    } else if (pu.isVecType) {
      (pd.filter(_.isVecRs).head, pu)
    } else if (pu.isIntType) {
      (pd.filter(_.isIntRs).head, pu)
    } else {
      (pd.filter(_.isMemRs).head, pu)
    }
  }

  override def bundleO(eo: (RsParam, ExuConfig)): IssueBundle = new IssueBundle(eo._1.bankNum, eo._1.entriesNum)
}
class ExuComplexIssueNode(implicit valName: ValName) extends
  MixedNexusNode(inner = ExuComplexIssueInwardNodeImpl, outer = ExuComplexIssueOutwardNodeImpl)(
    dFn = {p:Seq[Seq[RsParam]] => {
      require(p.length == 1)
      p.head
    }},
    uFn = {p:Seq[ExuConfig] => ExuComplexParam(p.head.id, p)}
  )

class ExuComplexWritebackNode(implicit valName: ValName) extends
  AdapterNode(ExuOutwardImpl)({p => p}, {p => p})