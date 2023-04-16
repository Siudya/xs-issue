package regfile

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import common._
import exu.{ExuConfig, ExuType}
import issue.RsParam
import xs.utils.SignExt

class ImmExtractor(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val in = Input(new ExuInput)
    val out = Output(new ExuInput)
  })
  io.out := io.in
}

class JumpImmExtractor(implicit p: Parameters) extends ImmExtractor {
  val jump_pc = IO(Input(UInt(VAddrBits.W)))
  val jalr_target = IO(Input(UInt(VAddrBits.W)))

  io.out.uop.cf.pc := jump_pc
  // when src1 is reg (like sfence's asid) do not let data_out(1) be the jalr_target
  when (SrcType.isPcOrImm(io.in.uop.ctrl.srcType(1))) {
    io.out.src(1) := jalr_target
  }
}

class AluImmExtractor(implicit p: Parameters) extends ImmExtractor {
  when (SrcType.isImm(io.in.uop.ctrl.srcType(1))) {
    val imm32 = Mux(io.in.uop.ctrl.selImm === SelImm.IMM_U,
      ImmUnion.U.toImm32(io.in.uop.ctrl.imm),
      ImmUnion.I.toImm32(io.in.uop.ctrl.imm)
    )
    io.out.src(1) := SignExt(imm32, XLEN)
  }
}

class BkuImmExtractor(implicit p: Parameters) extends ImmExtractor {
  when (SrcType.isImm(io.in.uop.ctrl.srcType(1))) {
    val imm32 = ImmUnion.I.toImm32(io.in.uop.ctrl.imm)
    io.out.src(1) := SignExt(imm32, XLEN)
  }
}

class LoadImmExtractor(implicit p: Parameters) extends ImmExtractor {
  when (SrcType.isImm(io.in.uop.ctrl.srcType(0))) {
    io.out.src(0) := SignExt(Imm_LUI_LOAD().getLuiImm(io.in.uop), XLEN)
  }
}

object ImmExtractor {
  def apply(cfg: ExuConfig, in: ExuInput, pc: Option[UInt], target: Option[UInt])
           (implicit p: Parameters): ExuInput = {
    val immExt = if (cfg.exuType == ExuType.jmp) {
      val ext = Module(new JumpImmExtractor)
      ext.jump_pc := pc.get
      ext.jalr_target := target.get
      ext
    } else if (cfg.exuType == ExuType.alu) {
      Module(new AluImmExtractor)
    } else if (cfg.exuType == ExuType.mul) {
      Module(new BkuImmExtractor)
    } else if (cfg.exuType == ExuType.load) {
      Module(new LoadImmExtractor)
    } else {
      Module(new ImmExtractor)
    }
    immExt.io.in := in
    immExt.io.out
  }
}