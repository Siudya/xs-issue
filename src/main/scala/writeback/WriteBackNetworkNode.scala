package writeback
import chipsalliance.rocketchip.config
import freechips.rocketchip.diplomacy._
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util._
import common.{ExuInput, ExuOutput, MicroOp}
import exu.ExuConfig
import issue.IssueBundle
object WriteBackNetworkNode()
