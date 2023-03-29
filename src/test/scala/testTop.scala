import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import issue.AllocateNetwork

object GenRtl extends App {
  (new chisel3.stage.ChiselStage).execute(args,
    Seq(
      ChiselGeneratorAnnotation(() => new AllocateNetwork(4, 12, 2))
    )
  )
}