import chisel3.stage.ChiselGeneratorAnnotation
import issue.IntRs.IntegerStatusArray
import issue.{AllocateNetwork, FuType, SelectNetwork}
import xs.utils.Assertion

object GenRtl extends App {
  val fuTypeList = Seq(FuType.mul, FuType.div)
  Assertion.set_enable(false)
  (new chisel3.stage.ChiselStage).execute(args,
    Seq(
      ChiselGeneratorAnnotation(() => new AllocateNetwork(4, 12))
    )
  )
  (new chisel3.stage.ChiselStage).execute(args,
    Seq(
      ChiselGeneratorAnnotation(() => new SelectNetwork(4, 4, 2, fuTypeList))
    )
  )
  (new chisel3.stage.ChiselStage).execute(args,
    Seq(
      ChiselGeneratorAnnotation(() => new IntegerStatusArray(12, 3, 4, 3 ))
    )
  )
}