package regfile

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import common._
import exu.{ExuConfig, ExuType}
import issue.RsParam
import xs.utils.SignExt

object ImmExtractor extends XSParam {
  def apply(cfg: ExuConfig, in: ExuInput, pc: Option[UInt], target: Option[UInt])
           (implicit p: Parameters): ExuInput = {
    if (cfg.exuType == ExuType.jmp) {
      JumpImmExtractor(in, pc.get, target.get)
    } else if (cfg.exuType == ExuType.alu) {
      AluImmExtractor(in)
    } else if (cfg.exuType == ExuType.mul) {
      BkuImmExtractor(in)
    } else if (cfg.exuType == ExuType.load) {
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