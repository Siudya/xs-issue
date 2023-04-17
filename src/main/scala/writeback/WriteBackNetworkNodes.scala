package writeback
import chipsalliance.rocketchip.config
import freechips.rocketchip.diplomacy._
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util._
import common.{ExuInput, ExuOutput, MicroOp}
import exu.ExuConfig
import issue.IssueBundle

object WriteBackSinkType{
  def intRf = 0
  def fpRf = 1
  def rob = 3
  def intRs = 4
  def memRs = 5
  def fpRs = 6
  private def rfList = Seq(intRf, fpRf)
  private def rsList = Seq(intRs, memRs, fpRs)
  def isRf(in:Int) = rfList.contains(in)
  def isRs(in:Int) = rsList.contains(in)
}

case class WriteBackSinkParam
(
  val name: String,
  val sinkType: Int,
){
  def isIntRf = sinkType == WriteBackSinkType.intRf
  def isFpRf = sinkType == WriteBackSinkType.fpRf
  def isRob = sinkType == WriteBackSinkType.rob
  def isIntRs = sinkType == WriteBackSinkType.intRs
  def isMemRs = sinkType == WriteBackSinkType.memRs
  def isFpRs = sinkType == WriteBackSinkType.fpRs
  def isLegal = isIntRf || isFpRf ||isRob ||isIntRs ||isMemRs ||isFpRs
  def needWakeup = isIntRs || isMemRs || isFpRs
  def needWriteback = isIntRf || isFpRf || isRob
}

object WriteBackNetworkNodeInwardImpl extends InwardNodeImp[ExuConfig, Option[ExuConfig], ExuConfig, Valid[ExuOutput]]{
  override def edgeI(pd: ExuConfig, pu: Option[ExuConfig], p: config.Parameters, sourceInfo: SourceInfo): ExuConfig = pd
  override def bundleI(ei: ExuConfig): ValidIO[ExuOutput] = Valid(new ExuOutput)
  override def render(e: ExuConfig): RenderedEdge = RenderedEdge("#0000ff",e.name + " writeback")
}
object WriteBackNetworkNodeOutwardImpl extends OutwardNodeImp[Seq[ExuConfig], WriteBackSinkParam, (WriteBackSinkParam, Seq[ExuConfig]), Vec[Valid[ExuOutput]]]{
  override def edgeO(pd: Seq[ExuConfig], pu: WriteBackSinkParam, p: config.Parameters, sourceInfo: SourceInfo): (WriteBackSinkParam, Seq[ExuConfig]) = {
    require(pu.isLegal)

    val resPd = if (pu.isIntRf) {
      pd.filter(_.writeIntRf)
    } else if (pu.isFpRf) {
      pd.filter(_.writeFpRf)
    } else if (pu.isIntRs) {
      pd.filter(_.wakeUpIntRs)
    } else if (pu.isMemRs) {
      pd.filter(_.wakeUpMemRs)
    } else if (pu.isFpRs) {
      pd.filter(_.wakeUpFpRs)
    } else {
      pd
    }
    (pu,pd)
  }
  override def bundleO(eo: (WriteBackSinkParam, Seq[ExuConfig])): Vec[ValidIO[ExuOutput]] = Vec(eo._2.length, Valid(new ExuOutput))
}
object WriteBackSinkNodeImpl extends SimpleNodeImp[Seq[ExuConfig], WriteBackSinkParam, Seq[ExuConfig], Vec[Valid[ExuOutput]]]{
  override def edge(pd: Seq[ExuConfig], pu: WriteBackSinkParam, p: config.Parameters, sourceInfo: SourceInfo): Seq[ExuConfig] = pd
  override def bundle(e: Seq[ExuConfig]): Vec[ValidIO[ExuOutput]] = Vec(e.length, Valid(new ExuOutput))
  override def render(e: Seq[ExuConfig]): RenderedEdge = RenderedEdge(colour = "#0000ff",label = "writeback")
}

class WriteBackNetworkNode(implicit valName: ValName) extends MixedNexusNode(WriteBackNetworkNodeInwardImpl, WriteBackNetworkNodeOutwardImpl)(
  dFn = {p:Seq[ExuConfig] => p},
  uFn = {p:Seq[WriteBackSinkParam] => None},
  inputRequiresOutput = false,
  outputRequiresInput = false
)
class WriteBackSinkNode(param:WriteBackSinkParam)(implicit valName: ValName) extends SinkNode(WriteBackSinkNodeImpl)(Seq(param))