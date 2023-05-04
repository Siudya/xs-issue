package execute.exublock

import chipsalliance.rocketchip.config.Parameters
import execute.exucx.{LduComplex, StaComplex, StdComplex, StdMouComplex}
import freechips.rocketchip.diplomacy.LazyModule

class MemoryBlock (loadNum:Int, storeNum:Int, atomicNum:Int)(implicit p:Parameters) extends BasicExuBlock{
  val ldus = Seq.tabulate(loadNum)(idx => LazyModule(new LduComplex(idx)))
  val stas = Seq.tabulate(storeNum)(idx => LazyModule(new StaComplex(idx)))
  val stds = Seq.tabulate(storeNum - atomicNum)(idx => LazyModule(new StdComplex(idx)))
  val stdmous = Seq.tabulate(atomicNum)(idx => LazyModule(new StdMouComplex(idx)))
  val memComplexes = ldus ++ stas ++ stds ++ stdmous

  memComplexes.foreach(exucx => {
    exucx.issueNode :*= issueNode
    writebackNode :=* exucx.writebackNode
  })
  lazy val module = new MemoryBlockImpl(this)
}

class MemoryBlockImpl(outer:MemoryBlock) extends BasicExuBlockImp(outer){
  outer.memComplexes.foreach(_.module.redirectIn := redirectIn)
}