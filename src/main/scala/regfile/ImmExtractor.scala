package regfile

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import common._
import execute.exucx.ExuComplexParam
import execute.exu.ExuType
import xs.utils.SignExt

object ImmExtractor extends XSParam {
  def apply(cfg: ExuComplexParam, in: ExuInput, pc: Option[UInt], target: Option[UInt])
           (implicit p: Parameters): ExuInput = {
    if (cfg.hasJmp) {
      Mux(in.uop.ctrl.fuType === FuType.jmp, JumpImmExtractor(in, pc.get, target.get), AluImmExtractor(in))
    } else if (cfg.hasMul) {
      Mux(in.uop.ctrl.fuType === FuType.bku, BkuImmExtractor(in), AluImmExtractor(in))
    } else if (cfg.hasDiv) {
      AluImmExtractor(in)
    } else if (cfg.hasLoad) {
      LoadImmExtractor(in)
    } else {
      in
    }
  }
  private def JumpImmExtractor(in:ExuInput, jump_pc:UInt, jalr_target:UInt):ExuInput = {
    val immExtractedRes = WireInit(in)
    immExtractedRes.uop.cf.pc := jump_pc
    // when src1 is reg (like sfence's asid) do not let data_out(1) be the jalr_target
    when(SrcType.isPcOrImm(in.uop.ctrl.srcType(1))) {
      immExtractedRes.src(1) := jalr_target
    }
    immExtractedRes
  }
  private def AluImmExtractor(in:ExuInput):ExuInput = {
    val immExtractedRes = WireInit(in)
    when(SrcType.isImm(in.uop.ctrl.srcType(1))) {
      val imm32 = Mux(in.uop.ctrl.selImm === SelImm.IMM_U,
        ImmUnion.U.toImm32(in.uop.ctrl.imm),
        ImmUnion.I.toImm32(in.uop.ctrl.imm)
      )
      immExtractedRes.src(1) := SignExt(imm32, XLEN)
    }
    immExtractedRes
  }
  private def BkuImmExtractor(in: ExuInput): ExuInput = {
    val immExtractedRes = WireInit(in)
    when(SrcType.isImm(in.uop.ctrl.srcType(1))) {
      val imm32 = ImmUnion.I.toImm32(in.uop.ctrl.imm)
      immExtractedRes.src(1) := SignExt(imm32, XLEN)
    }
    immExtractedRes
  }
  private def LoadImmExtractor(in: ExuInput): ExuInput = {
    val immExtractedRes = WireInit(in)
    when(SrcType.isImm(in.uop.ctrl.srcType(0))) {
      immExtractedRes.src(0) := SignExt(Imm_LUI_LOAD().getLuiImm(in.uop), XLEN)
    }
    immExtractedRes
  }
}