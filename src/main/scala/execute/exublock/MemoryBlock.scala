package execute.exublock

import chipsalliance.rocketchip.config.Parameters
import execute.exu.{ExuConfig, ExuInputNode, ExuOutputNode, ExuType}
import execute.exucx.{ExuComplexIssueNode, ExuComplexWritebackNode}
import execute.fu.FuConfigs

class MemoryBlock (loadNum:Int, storeNum:Int, atomicNum:Int)(implicit p:Parameters) extends BasicExuBlock{
  private val lduParams = Seq.tabulate(loadNum)(idx => {
    ExuConfig(
      name = "LduExu",
      id = idx,
      complexName = "MemoryBlock",
      fuConfigs = Seq(FuConfigs.lduCfg),
      exuType = ExuType.ldu
    )
  })
  private val staParams = Seq.tabulate(storeNum)(idx => {
    ExuConfig(
      name = "StaExu",
      id = idx,
      complexName = "MemoryBlock",
      fuConfigs = Seq(FuConfigs.staCfg),
      exuType = ExuType.sta
    )
  })
  private val stdParams = Seq.tabulate(storeNum - atomicNum)(idx => {
    ExuConfig(
      name = "StdExu",
      id = idx,
      complexName = "MemoryBlock",
      fuConfigs = Seq(FuConfigs.stdCfg),
      exuType = ExuType.std
    )
  })
  private val stdMouParams = Seq.tabulate(storeNum - atomicNum)(idx => {
    ExuConfig(
      name = "StdMouExu",
      id = idx,
      complexName = "MemoryBlock",
      fuConfigs = Seq(FuConfigs.stdCfg, FuConfigs.mouCfg, FuConfigs.mouDataCfg),
      exuType = ExuType.mou
    )
  })
  protected[exublock] val lduIssueNodes = lduParams.zipWithIndex.map(new MemoryBlockIssueNode(_))
  protected[exublock] val lduWritebackNodes = lduParams.map(new ExuOutputNode(_))
  protected[exublock] val staIssueNodes = staParams.zipWithIndex.map(new MemoryBlockIssueNode(_))
  protected[exublock] val staWritebackNodes = staParams.map(new ExuOutputNode(_))
  protected[exublock] val stdIssueNodes = stdParams.zipWithIndex.map(new MemoryBlockIssueNode(_))
  protected[exublock] val stdWritebackNodes = stdParams.map(new ExuOutputNode(_))
  protected[exublock] val stdMouIssueNodes = stdMouParams.zipWithIndex.map(new MemoryBlockIssueNode(_))
  protected[exublock] val stdMouWritebackNodes = stdMouParams.map(new ExuOutputNode(_))
  private val allIssueNodes = lduIssueNodes ++ staIssueNodes ++ stdIssueNodes ++ stdMouIssueNodes
  private val allWritebackNodes = lduWritebackNodes ++ staWritebackNodes ++ stdWritebackNodes ++ stdMouWritebackNodes

  allIssueNodes.foreach(inode => inode :*= issueNode)
  allWritebackNodes.foreach(onode => writebackNode :=* onode)

  lazy val module = new MemoryBlockImpl(this)
}

class MemoryBlockImpl(outer:MemoryBlock) extends BasicExuBlockImp(outer){

  private val lduIssues = outer.lduIssueNodes.map(iss => {
    require(iss.in.length == 1)
    iss.in.head
  })
  private val staIssues = outer.staIssueNodes.map(iss => {
    require(iss.in.length == 1)
    iss.in.head
  })
  private val stdIssues = outer.stdIssueNodes.map(iss => {
    require(iss.in.length == 1)
    iss.in.head
  })
  private val stdMouIssues = outer.stdMouIssueNodes.map(iss => {
    require(iss.in.length == 1)
    iss.in.head
  })
  private val lduWritebacks = outer.lduWritebackNodes.map(wb => {
    require(wb.out.length == 1)
    wb.out.head
  })
  private val staWritebacks = outer.staWritebackNodes.map(wb => {
    require(wb.out.length == 1)
    wb.out.head
  })
  private val stdWritebacks = outer.stdWritebackNodes.map(wb => {
    require(wb.out.length == 1)
    wb.out.head
  })
  private val stdMouWritebacks = outer.stdMouWritebackNodes.map(wb => {
    require(wb.out.length == 1)
    wb.out.head
  })

}