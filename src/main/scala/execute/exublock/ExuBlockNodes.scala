package execute.exublock

import chipsalliance.rocketchip.config
import chisel3.internal.sourceinfo.SourceInfo
import execute.exucx.{ExuComplexParam, ExuComplexWritebackNode}
import freechips.rocketchip.diplomacy.{AdapterNode, RenderedEdge, SimpleNodeImp, ValName}
import issue.{IssueBundle, RsParam}

object ExuBlockIssueNodeImpl extends SimpleNodeImp[Seq[RsParam], ExuComplexParam, (RsParam, ExuComplexParam), IssueBundle]{
  override def edge(pd: Seq[RsParam], pu: ExuComplexParam, p: config.Parameters, sourceInfo: SourceInfo): (RsParam, ExuComplexParam) = {
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
  override def bundle(e: (RsParam, ExuComplexParam)): IssueBundle = new IssueBundle(e._1.bankNum, e._1.entriesNum)
  override def render(e: (RsParam, ExuComplexParam)): RenderedEdge = RenderedEdge("#0000ff", e._1.TypeName + "Issue")
}

class ExuBlockWritebackNode(implicit valName: ValName) extends ExuComplexWritebackNode

class ExuBlockIssueNode(implicit valName: ValName) extends
  AdapterNode(ExuBlockIssueNodeImpl)({p => p}, {p => p})
