package regfile
import chipsalliance.rocketchip.config
import freechips.rocketchip.diplomacy.{MixedNexusNode, _}
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import execute.exucx.ExuComplexParam
import issue.{IssueBundle, RsParam}

object RegFileNodeInwardImpl extends InwardNodeImp[RsParam,Seq[ExuComplexParam],(RsParam, Seq[ExuComplexParam]),Vec[IssueBundle]]{

  override def edgeI(pd: RsParam, pu: Seq[ExuComplexParam], p: config.Parameters, sourceInfo: SourceInfo): (RsParam, Seq[ExuComplexParam]) = {
    require(pd.isLegal)
    if(pd.isIntRs){
      (pd, pu.filter(_.isIntType))
    } else if(pd.isMemRs) {
      (pd, pu.filter(_.isMemType))
    } else if(pd.isVecRs) {
      (pd, pu.filter(_.isVecType))
    } else {
      (pd, pu.filter(_.isFpType))
    }
  }
  override def bundleI(ei: (RsParam, Seq[ExuComplexParam])): Vec[IssueBundle] = Vec(ei._2.length, new IssueBundle(ei._1.bankNum, ei._1.entriesNum))
  override def render(e: (RsParam, Seq[ExuComplexParam])): RenderedEdge = RenderedEdge("#0000ff", e._1.TypeName + "Issue")
}
object RegFileNodeOutwardImpl extends OutwardNodeImp[Seq[RsParam], ExuComplexParam, (RsParam, ExuComplexParam), IssueBundle]{
  override def edgeO(pd: Seq[RsParam], pu: ExuComplexParam, p: config.Parameters, sourceInfo: SourceInfo): (RsParam, ExuComplexParam) = {
    require(pu.isFpType || pu.isVecType || pu.isIntType || pu.isMemType)
    if(pu.isFpType){
      (pd.filter(_.isFpRs).head, pu)
    } else if(pu.isVecType) {
      (pd.filter(_.isVecRs).head, pu)
    } else if (pu.isIntType) {
      (pd.filter(_.isIntRs).head, pu)
    } else {
      (pd.filter(_.isMemRs).head, pu)
    }
  }
  override def bundleO(eo: (RsParam, ExuComplexParam)): IssueBundle = new IssueBundle(eo._1.bankNum, eo._1.entriesNum)
}

class RegFileNode(implicit valName: ValName) extends MixedNexusNode(
  inner = RegFileNodeInwardImpl, outer = RegFileNodeOutwardImpl
)(
  { pd => pd }, { pu => pu }
)