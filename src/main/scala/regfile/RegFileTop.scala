package regfile
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import chipsalliance.rocketchip.config.Parameters
import chisel3.experimental.prefix
import common.{ExuInput, MicroOp, Redirect, XSParam}
import execute.fu.fpu.FMAMidResult
import writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}

class RegFileTop(implicit p:Parameters) extends LazyModule{
  val issueNode = new RegFileNode
  val writebackNode = new WriteBackSinkNode(WriteBackSinkParam("RegFile Top", WriteBackSinkType.regFile))

  lazy val module = new LazyModuleImp(this) with XSParam {
    val pcReadNum:Int = issueNode.out.count(_._2._2.hasJmp) * 2 + issueNode.out.count(_._2._2.hasLoad)
    val io = IO(new Bundle{
      val redirect = Input(Valid(new Redirect))
      val pcReadFtqIdx = Output(Vec(pcReadNum, UInt(log2Ceil(FtqSize).W)))
      val pcReadFtqOffset = Output(Vec(pcReadNum, UInt(PredictWidth.W)))
      val pcReadData = Input(Vec(pcReadNum, UInt(VAddrBits.W)))
      val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    })
    require(issueNode.in.count(_._2._1.isIntRs) <= 1)
    require(issueNode.in.count(_._2._1.isMemRs) <= 1)
    require(issueNode.in.count(_._2._1.isFpRs) <= 1)
    require(writebackNode.in.length == 1)
    private val wb = writebackNode.in.flatMap(i => i._1.zip(i._2))

    private val fromRs = issueNode.in.flatMap(i => i._1.zip(i._2._2).map(e => (e._1, e._2, i._2._1)))
    private val toExuMap = issueNode.out.map(i => i._2._2 -> (i._1, i._2._2, i._2._1)).toMap

    private val needIntSrc = issueNode.out.filter(i => i._2._2.readIntegerRegfile).map(i => (i._1, i._2._2, i._2._1))
    private val needFpSrc = issueNode.out.filter(i => i._2._2.readFloatingRegfile).map(i => (i._1, i._2._2, i._2._1))

    private val writeIntRfBypass = wb.filter(i => i._2.bypassIntRegfile)
    private val writeIntRf = wb.filterNot(i => i._2.bypassIntRegfile && i._2.writeIntRf)
    private val writeFpRfBypass = wb.filter(i => i._2.bypassFpRegfile)
    private val writeFpRf = wb.filterNot(i => i._2.bypassFpRegfile && i._2.writeFpRf)
    private val fmacFeedbacks = needFpSrc.filter(_._2.hasFmac).map(_._1)

    private val intRf = Module(new GenericRegFile(GprEntriesNum, writeIntRf.length, writeIntRfBypass.length, needIntSrc.map(_._2.intSrcNum).sum, XLEN, "IntegerRegFile"))
    private val fpRf = Module(new GenericRegFile(FprEntriesNum, writeFpRf.length, writeFpRfBypass.length, needFpSrc.map(_._2.fpSrcNum).sum, XLEN, "FloatingRegFile", fmacFeedbacks.length))

    private val intWriteBackSinks = intRf.io.write ++ intRf.io.bypassWrite
    private val intWriteBackSources = writeIntRf ++ writeIntRfBypass
    intWriteBackSinks.zip(intWriteBackSources.map(_._1)).foreach({case(sink, source) =>
      sink.en := source.valid && !source.bits.uop.robIdx.needFlush(io.redirect) && source.bits.uop.ctrl.rfWen
      sink.addr := source.bits.uop.pdest
      sink.data := source.bits.data
    })

    private val fpWriteBackSinks = fpRf.io.write ++ fpRf.io.bypassWrite
    private val fpWriteBackSources = writeFpRf ++ writeFpRfBypass
    fpWriteBackSinks.zip(fpWriteBackSources.map(_._1)).foreach({ case (sink, source) =>
      sink.en := source.valid && !source.bits.uop.robIdx.needFlush(io.redirect) && source.bits.uop.ctrl.fpWen
      sink.addr := source.bits.uop.pdest
      sink.data := source.bits.data
    })

    fpRf.io.extraWrite.zip(fmacFeedbacks.map(_.fmaMidState.out)).foreach({case(sink, source) =>
      sink.en := source.valid
      sink.addr := source.bits.pdest
      sink.data := source.bits.midResult.asUInt(XLEN - 1, 0)
    })

    private var intRfReadIdx = 0
    private var fpRfReadIdx = 0
    private var pcReadPortIdx = 0

    for(in <- fromRs){
      val out = toExuMap(in._2)
      val rsParam = in._3
      val exuComplexParam = in._2
      val bi = in._1
      val bo = out._1
      prefix(s"${exuComplexParam.name}_${exuComplexParam.id}") {
        val exuInBundle = WireInit(bi.issue.bits)
        val midResultBundle = WireInit(bi.fmaMidState.in)
        exuInBundle.src := DontCare

        val lpvNeedCancel = bi.issue.bits.uop.lpv.zip(io.earlyWakeUpCancel).map({case(l,c) => l(1) & c}).reduce(_|_)
        if (exuComplexParam.isIntType) {
          val srcNum = exuComplexParam.intSrcNum
          for((d, addr) <- exuInBundle.src.zip(exuInBundle.uop.psrc).take(srcNum)){
            intRf.io.read(intRfReadIdx).addr := addr
            d := Mux(addr === 0.U, 0.U, intRf.io.read(intRfReadIdx).data)
            intRfReadIdx = intRfReadIdx + 1
          }
          if(exuComplexParam.hasJmp){
            io.pcReadFtqIdx(pcReadPortIdx) := exuInBundle.uop.cf.ftqPtr.value
            io.pcReadFtqIdx(pcReadPortIdx + 1) := (exuInBundle.uop.cf.ftqPtr + 1.U).value
            io.pcReadFtqOffset(pcReadPortIdx) := exuInBundle.uop.cf.ftqOffset
            io.pcReadFtqOffset(pcReadPortIdx + 1) := 0.U
            ImmExtractor(exuComplexParam, exuInBundle, Some(io.pcReadData(pcReadPortIdx)), Some(io.pcReadData(pcReadPortIdx + 1)))
            pcReadPortIdx = pcReadPortIdx + 2
          } else {
            ImmExtractor(exuComplexParam, exuInBundle)
          }
        } else if(exuComplexParam.isFpType){
          val srcNum = exuComplexParam.fpSrcNum
          for (((d, addr), idx) <- exuInBundle.src.zip(exuInBundle.uop.psrc).take(srcNum).zipWithIndex) {
            val realAddr = if(idx != 0 || !exuComplexParam.hasFmac) addr else Mux(bi.fmaMidState.in.valid, exuInBundle.uop.pdest, addr)
            fpRf.io.read(fpRfReadIdx).addr := realAddr
            d := fpRf.io.read(fpRfReadIdx).data
            fpRfReadIdx = fpRfReadIdx + 1
          }
          val midResultWidth = (new FMAMidResult).getWidth
          val midResultLo = exuInBundle.src(0)
          val midResultHi = bi.fmaMidState.in.bits.asUInt(midResultWidth - 1, XLEN)
          midResultBundle.bits := Cat(midResultHi, midResultLo).asTypeOf(new FMAMidResult)
        }

        val issueValidReg = RegInit(false.B)
        val issueExuInReg = Reg(new ExuInput)
        val midResultReg = Reg(Valid(new FMAMidResult))
        val fmaWaitAdd = Reg(Bool())

        val allowPipe = !issueValidReg || bo.issue.fire
        bi.fuInFire := bo.issue.fire
        bo.issue.valid := issueValidReg
        bo.issue.bits := issueExuInReg
        bo.fmaMidState.in := midResultReg
        bo.fmaMidState.waitForAdd := fmaWaitAdd
        when(allowPipe) {
          issueValidReg := bi.issue.valid && !bi.issue.bits.uop.robIdx.needFlush(io.redirect) && !lpvNeedCancel
        }
        when(allowPipe && bi.issue.valid) {
          issueExuInReg := exuInBundle
          midResultReg := midResultBundle
          fmaWaitAdd := bi.fmaMidState.waitForAdd
        }
      }
    }
  }
}
