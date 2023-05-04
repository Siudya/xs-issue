package execute.exublock

import chipsalliance.rocketchip.config.Parameters
import execute.exucx.{LduComplex, StaComplex, StdComplex}
import freechips.rocketchip.diplomacy.LazyModule

class MemoryBlock (loadNum:Int, storeNum:Int)(implicit p:Parameters) extends BasicExuBlock{
  val ldus = Seq.tabulate(loadNum)(idx => LazyModule(new LduComplex(idx)))
  val stas = Seq.tabulate(storeNum)(idx => LazyModule(new StaComplex(idx)))
  val stds = Seq.tabulate(storeNum)(idx => LazyModule(new StdComplex(idx)))
  val memComplexes = ldus ++ stas ++ stds

  memComplexes.foreach(exucx => {
    exucx.issueNode :*= issueNode
    writebackNode :=* exucx.writebackNode
  })
  lazy val module = new MemoryBlockImpl(this)
}

class MemoryBlockImpl(outer:MemoryBlock) extends BasicExuBlockImp(outer){
  outer.memComplexes.foreach(_.module.redirectIn := redirectIn)
}