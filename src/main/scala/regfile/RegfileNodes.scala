package regfile
import chipsalliance.rocketchip.config
import freechips.rocketchip.diplomacy._
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import execute.exucx.ExuComplexParam
import issue.{IssueBundle, RsParam}

object RegfileInwardImpl extends InwardNodeImp[RsParam,Seq[ExuComplexParam],Seq[ExuComplexParam],Vec[IssueBundle]]{

  override def edgeI(pd: RsParam, pu: Seq[ExuComplexParam], p: config.Parameters, sourceInfo: SourceInfo): Seq[ExuComplexParam] = {
    require(pd.isLegal)
    if(pd.isIntRs){
      pu.filter(_.isIntType)
    } else if(pd.isMemRs) {
      pu.filter(_.isMemType)
    } else {
      pu.filter(_.isFpType)
    }
  }

  override def bundleI(ei: Seq[ExuComplexParam]): Vec[IssueBundle] = Vec(ei.length, new IssueBundle)

  override def render(e: Seq[ExuComplexParam]): RenderedEdge = {
    val edgeName = if(e.head.isIntType)"Int" else if(e.head.isFpType) "Fp" else "Mem"
    RenderedEdge("#0000ff", edgeName + "Issue")
  }
}
object RegfileOutwardImpl extends OutwardNodeImp[Option[RsParam],ExuComplexParam,ExuComplexParam,IssueBundle]{

  override def edgeO(pd: Option[RsParam], pu: ExuComplexParam, p: config.Parameters, sourceInfo: SourceInfo): ExuComplexParam = pu

  override def bundleO(eo: ExuComplexParam): IssueBundle = new IssueBundle
}

class RegfileIssueNode(implicit valName: ValName)
  extends MixedNexusNode(
    inner = RegfileInwardImpl, outer = RegfileOutwardImpl
  )(
    {p:Seq[RsParam] => None}, {p:Seq[ExuComplexParam] =>p}
  )
