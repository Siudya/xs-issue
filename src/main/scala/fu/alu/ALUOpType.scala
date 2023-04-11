package fu.alu
import chisel3._
import chisel3.util._

object ALUOpType {
  // shift optype
  def slliuw = "b000_0000".U // slliuw: ZEXT(src1[31:0]) << shamt
  def sll = "b000_0001".U // sll:     src1 << src2
  def bclr = "b000_0010".U // bclr:    src1 & ~(1 << src2[5:0])
  def bset = "b000_0011".U // bset:    src1 | (1 << src2[5:0])
  def binv = "b000_0100".U // binv:    src1 ^ ~(1 << src2[5:0])
  def srl = "b000_0101".U // srl:     src1 >> src2
  def bext = "b000_0110".U // bext:    (src1 >> src2)[0]
  def sra = "b000_0111".U // sra:     src1 >> src2 (arithmetic)
  def rol = "b000_1001".U // rol:     (src1 << src2) | (src1 >> (xlen - src2))
  def ror = "b000_1011".U // ror:     (src1 >> src2) | (src1 << (xlen - src2))
  // RV64 32bit optype
  def addw = "b001_0000".U // addw:      SEXT((src1 + src2)[31:0])
  def oddaddw = "b001_0001".U // oddaddw:   SEXT((src1[0] + src2)[31:0])
  def subw = "b001_0010".U // subw:      SEXT((src1 - src2)[31:0])
  def addwbit = "b001_0100".U // addwbit:   (src1 + src2)[0]
  def addwbyte = "b001_0101".U // addwbyte:  (src1 + src2)[7:0]
  def addwzexth = "b001_0110".U // addwzexth: ZEXT((src1  + src2)[15:0])
  def addwsexth = "b001_0111".U // addwsexth: SEXT((src1  + src2)[15:0])
  def sllw = "b001_1000".U // sllw:     SEXT((src1 << src2)[31:0])
  def srlw = "b001_1001".U // srlw:     SEXT((src1[31:0] >> src2)[31:0])
  def sraw = "b001_1010".U // sraw:     SEXT((src1[31:0] >> src2)[31:0])
  def rolw = "b001_1100".U
  def rorw = "b001_1101".U
  // ADD-op
  def adduw = "b010_0000".U // adduw:  src1[31:0]  + src2
  def add = "b010_0001".U // add:     src1        + src2
  def oddadd = "b010_0010".U // oddadd:  src1[0]     + src2
  def sr29add = "b010_0100".U // sr29add: src1[63:29] + src2
  def sr30add = "b010_0101".U // sr30add: src1[63:30] + src2
  def sr31add = "b010_0110".U // sr31add: src1[63:31] + src2
  def sr32add = "b010_0111".U // sr32add: src1[63:32] + src2
  def sh1adduw = "b010_1000".U // sh1adduw: {src1[31:0], 1'b0} + src2
  def sh1add = "b010_1001".U // sh1add: {src1[62:0], 1'b0} + src2
  def sh2adduw = "b010_1010".U // sh2add_uw: {src1[31:0], 2'b0} + src2
  def sh2add = "b010_1011".U // sh2add: {src1[61:0], 2'b0} + src2
  def sh3adduw = "b010_1100".U // sh3add_uw: {src1[31:0], 3'b0} + src2
  def sh3add = "b010_1101".U // sh3add: {src1[60:0], 3'b0} + src2
  def sh4add = "b010_1111".U // sh4add: {src1[59:0], 4'b0} + src2
  // SUB-op: src1 - src2
  def sub = "b011_0000".U
  def sltu = "b011_0001".U
  def slt = "b011_0010".U
  def maxu = "b011_0100".U
  def minu = "b011_0101".U
  def max = "b011_0110".U
  def min = "b011_0111".U
  // branch
  def beq = "b111_0000".U
  def bne = "b111_0010".U
  def blt = "b111_1000".U
  def bge = "b111_1010".U
  def bltu = "b111_1100".U
  def bgeu = "b111_1110".U
  // misc optype
  def and = "b100_0000".U
  def andn = "b100_0001".U
  def or = "b100_0010".U
  def orn = "b100_0011".U
  def xor = "b100_0100".U
  def xnor = "b100_0101".U
  def orcb = "b100_0110".U
  def sextb = "b100_1000".U
  def packh = "b100_1001".U
  def sexth = "b100_1010".U
  def packw = "b100_1011".U
  def revb = "b101_0000".U
  def rev8 = "b101_0001".U
  def pack = "b101_0010".U
  def orh48 = "b101_0011".U
  def szewl1 = "b101_1000".U
  def szewl2 = "b101_1001".U
  def szewl3 = "b101_1010".U
  def byte2 = "b101_1011".U
  def andlsb = "b110_0000".U
  def andzexth = "b110_0001".U
  def orlsb = "b110_0010".U
  def orzexth = "b110_0011".U
  def xorlsb = "b110_0100".U
  def xorzexth = "b110_0101".U
  def orcblsb = "b110_0110".U
  def orcbzexth = "b110_0111".U
  def isAddw(func: UInt) = func(6, 4) === "b001".U && !func(3) && !func(1)
  def isSimpleLogic(func: UInt) = func(6, 4) === "b100".U && !func(0)
  def logicToLsb(func: UInt) = Cat("b110".U(3.W), func(3, 1), 0.U(1.W))
  def logicToZexth(func: UInt) = Cat("b110".U(3.W), func(3, 1), 1.U(1.W))
  def isBranch(func: UInt) = func(6, 4) === "b111".U
  def getBranchType(func: UInt) = func(3, 2)
  def isBranchInvert(func: UInt) = func(1)
  def apply() = UInt(7.W)
}
