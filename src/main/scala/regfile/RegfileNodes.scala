package regfile
import chipsalliance.rocketchip.config
import freechips.rocketchip.diplomacy._
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util._
import exu.ExuConfig
import issue.{IssueBundle, RsParam}

object RegfileInwardImpl extends InwardNodeImp[RsParam,Seq[ExuConfig],Seq[ExuConfig],Vec[IssueBundle]]{

  override def edgeI(pd: RsParam, pu: Seq[ExuConfig], p: config.Parameters, sourceInfo: SourceInfo): Seq[ExuConfig] = {
    require(pd.isLegal)
    if(pd.isIntRs){
      pu.filter(_.isIntType)
    } else if(pd.isMemRs) {
      pu.filter(_.isMemType)
    } else {
      pu.filter(_.isFpType)
    }
  }

  override def bundleI(ei: Seq[ExuConfig]): Vec[IssueBundle] = Vec(ei.length, new IssueBundle)

  override def render(e: Seq[ExuConfig]): RenderedEdge = {
    val edgeName = if(e.head.isIntType)"Int" else if(e.head.isFpType) "Fp" else "Mem"
    RenderedEdge("#0000ff", edgeName + "Issue")
  }
}
object RegfileOutwardImpl extends OutwardNodeImp[Option[RsParam],ExuConfig,ExuConfig,IssueBundle]{

  override def edgeO(pd: Option[RsParam], pu: ExuConfig, p: config.Parameters, sourceInfo: SourceInfo): ExuConfig = pu

  override def bundleO(eo: ExuConfig): IssueBundle = new IssueBundle
}

class RegfileIssueNode(implicit valName: ValName)
  extends MixedNexusNode(
    inner = RegfileInwardImpl, outer = RegfileOutwardImpl
  )(
    {p:Seq[RsParam] => None}, {p:Seq[ExuConfig] =>p}
  )
