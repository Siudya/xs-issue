package execute.exublock

import chipsalliance.rocketchip.config.Parameters
import execute.exu.{ExuConfig, ExuInputNode, ExuOutputNode, ExuType}
import execute.exucx.{ExuComplexIssueNode, ExuComplexWritebackNode}
import execute.fu.FuConfigs
import chisel3._
import chisel3.util._
import common._

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


class memBlock_CtrlBlockBundle extends XSBundle{
  val enqLsq = new LsqEnqIO
}

class memBlockPFinfoBundle extends XSBundle{
  val loadPC = Input(UInt(VAddrBits.W))
}

class memBlockMiscBundle extends XSBundle{
  //  val ptw =
  val sfence = Input(new SfenceBundle)
  val tlbCsr = Input(new TlbCsrBundle)
  val fenceToSbuffer = new FenceToSbuffer()

  val lsqio = new Bundle() {
    val exceptionAddr = new ExceptionAddrIO // to csr
    val rob = Flipped(new RobLsqIO) // rob to lsq
  }

  val csrCtrl = Flipped(new CustomCSRCtrlIO)
  val csrUpdate = new DistributedCSRUpdateReq
  val error = new L1CacheErrorInfo
  val memInfo = new Bundle {
    val sqFull = Output(Bool())
    val lqFull = Output(Bool())
    val dcacheMSHRFull = Output(Bool())
  }

  val sqFull = Output(Bool())
  val lqFull = Output(Bool())
  val perfEventsPTW = Input(Vec(19, new PerfEvent))

  val s3_delayed_load_error = Output(Bool())
  val lqCancelCnt = Output(UInt(log2Up(loadQueueSize + 1).W))
  val sqCancelCnt = Output(UInt(log2Up(storeQueueSize + 1).W))
  val sqDeq = Output(UInt(2.W)) //to parameterize
}