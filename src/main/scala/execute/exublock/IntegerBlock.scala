package execute.exublock

import chipsalliance.rocketchip.config.Parameters
import execute.exucx.{AluDivComplex, AluI2fComplex, AluMulComplex, JmpCsrComplex}
import freechips.rocketchip.diplomacy.LazyModule
import chisel3._
import chisel3.util._
import execute.exu.FenceIO

class IntegerBlock(mulNum:Int, divNum:Int, i2fNum:Int, jmpNum:Int)(implicit p:Parameters) extends BasicExuBlock {
  require(jmpNum == 1)
  private val aluMuls = Seq.tabulate(mulNum)(idx => LazyModule(new AluMulComplex(idx, mulNum - 1)))
  private val aluDivs = Seq.tabulate(divNum)(idx => LazyModule(new AluDivComplex(idx, mulNum)))
  private val aluI2fs = Seq.tabulate(i2fNum)(idx => LazyModule(new AluI2fComplex(idx, mulNum)))
  private val jmps = Seq.tabulate(jmpNum)(idx => LazyModule(new JmpCsrComplex(idx, mulNum)))
  private val intComplexes = aluMuls ++ aluDivs ++ aluI2fs ++ jmps
  intComplexes.foreach(exucx => {
    exucx.issueNode :*= issueNode
    writebackNode :=* exucx.writebackNode
  })

  lazy val module = new BasicExuBlockImp(this){
    val io = IO(new Bundle {
      val fenceio = new FenceIO
    })
    intComplexes.foreach(_.module.redirectIn := redirectIn)

    (jmps ++ aluDivs ++ aluI2fs).foreach(cplx => cplx.module.bypassIn.zip(aluMuls.map(_.module.io.bypassOut)).foreach({ case (a, b) => a := b }))

    aluMuls.map(_.module.bypassIn).zipWithIndex.foreach({ case (bin, idx) =>
      val sources = aluMuls.zipWithIndex.filterNot(_._2 == idx).map(_._1.module.io.bypassOut)
      bin.zip(sources).foreach({ case (a, b) => a := b })
    })

    jmps.head.module.io.fenceio <> io.fenceio
  }
}
