import chisel3.stage.ChiselGeneratorAnnotation
import issue.IntRs.IntegerReservationStation
import issue.{FuType, RsParam}
import xs.utils.Assertion

object GenRtl extends App {
  val p = RsParam(
    16,
    3,
    Seq(
      (1, Seq(FuType.jmp, FuType.csr, FuType.fence, FuType.i2f)),
      (4, Seq(FuType.alu, FuType.bku)),
      (2, Seq(FuType.div, FuType.mul))
    )
  )
  Assertion.set_enable(false)
  (new chisel3.stage.ChiselStage).execute(args,
    Seq(
      ChiselGeneratorAnnotation(() => new IntegerReservationStation(p))
    )
  )
}