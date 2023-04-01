/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  ***************************************************************************************/
/***************************************************************************************
  * Author: Liang Sen
  * E-mail: liangsen20z@ict.ac.cn
  * Date: 2023-03-31
  ****************************************************************************************/
package issue
import chisel3._
import chisel3.util._
import xs.utils.Assertion.xs_assert
import xs.utils.ParallelOperation

class SelectResp(bankIdxWidth:Int, entryIdxWidth:Int) extends Bundle {
  val info = new SelectInfo
  val entryIdxOH = UInt(entryIdxWidth.W)
  val bankIdxOH = UInt(bankIdxWidth.W)
}
class SelectMux(bankIdxWidth:Int, entryIdxWidth:Int) extends Module{
  val io = IO(new Bundle{
    val in0 = Input(Valid(new SelectResp(bankIdxWidth, entryIdxWidth)))
    val in1 = Input(Valid(new SelectResp(bankIdxWidth, entryIdxWidth)))
    val out = Output(Valid(new SelectResp(bankIdxWidth, entryIdxWidth)))
  })
  private val valid0 = io.in0.valid
  private val valid1 = io.in1.valid
  private val ptr0 = io.in0.bits.info.robPtr
  private val ptr1 = io.in1.bits.info.robPtr
  private val validVec = Cat(valid1, valid0)
  private val sel = Mux(validVec === "b01".U, true.B, Mux(validVec === "b10".U, false.B, Mux(validVec === "b11".U, ptr0 < ptr1, true.B)))
  private val res = Mux(sel, io.in0, io.in1)
  io.out := res
}
object SelectMux{
  def apply(in0: Valid[SelectResp], in1: Valid[SelectResp], bankIdxWidth:Int, entryIdxWidth:Int):Valid[SelectResp] = {
    val smux = Module(new SelectMux(bankIdxWidth, entryIdxWidth))
    smux.io.in0 := in0
    smux.io.in1 := in1
    smux.io.out
  }
}

class Selector(bankNum:Int, entryNum:Int, inputWidth:Int) extends Module{
  private val bankIdxWidth = bankNum
  private val entryIdxWidth = entryNum
  val io = IO(new Bundle{
    val in = Input(Vec(inputWidth, Valid(new SelectResp(bankIdxWidth, entryIdxWidth))))
    val out = Output(Valid(new SelectResp(bankIdxWidth, entryIdxWidth)))
  })
  private val operationFunction = SelectMux(_, _, bankIdxWidth, entryIdxWidth)
  private val res  = ParallelOperation(io.in, operationFunction)
  io.out := res
}

/** {{{
  * Module Name: SelectedNetwork
  *
  * Function Description:
  *   Select ready and supported instructions from several
  *   reservation station banks.
  *
  * Parameters:
  *   bankNum:
  *     The number of banks.
  *   entryNum:
  *     The number of entries in a bank.
  *   issueNum:
  *     The number of issue port for a certain type of instructions.
  *   fuTypeList:
  *     The list of function unit types, to which the selected
  *     instructions should be sent.
  *
  * IO:
  *   selectInfo: [Input][Vec]
  *     The necessary information for selection, which comes from
  *     reservation station banks.
  *   issueInfo: [Output][Vec][Valid]
  *     info:
  *       The information of selected instruction.
  *     bankIdxOH:
  *       The one hot format bank index of selected instruction.
  *     entryIdxOH:
  *       The one hot format entry index of selected instruction.
  * }}}
*/

class SelectNetwork(bankNum:Int, entryNum:Int, issueNum:Int, fuTypeList:Seq[UInt]) extends Module {
  require(issueNum <= bankNum && 0 < issueNum && bankNum % issueNum == 0, "Illegal number of issue ports are supported now!")
  val io = IO(new Bundle{
    val selectInfo = Input(Vec(bankNum,Vec(entryNum, Valid(new SelectInfo))))
    val issueInfo = Output(Vec(issueNum, Valid(new SelectResp(bankNum, entryNum))))
  })

  private val issueValidBitVecList = io.selectInfo.map(_.map(info => info.valid & (Cat(fuTypeList.map(_ === info.bits.fuType)).orR)))
  private val issueDataVecList = io.selectInfo.map(_.map(_.bits))
  private val issueBankIdxVecList = io.selectInfo.indices.map(idx => Seq.fill(entryNum)((1<<idx).U(bankNum.W)))
  private val issueEntryIdxVecList = io.selectInfo.indices.map(idx => Seq.tabulate(entryNum)(idx0 => (1<<idx0).U(entryNum.W)))
  private val issueAllDataList = issueValidBitVecList.zip(issueDataVecList).zip(issueBankIdxVecList).zip(issueEntryIdxVecList).map({
    case(((v, d),bi),ei) => v.zip(d).zip(bi).zip(ei)
  })

  private val bankNumPerSelector = bankNum / issueNum
  private val selectorSeq = Seq.fill(issueNum)(Module(new Selector(bankNum, entryNum, bankNumPerSelector * entryNum)))

  private val selectorInput = Seq.tabulate(issueNum)({idx =>
    issueAllDataList.slice(idx*bankNumPerSelector, idx*bankNumPerSelector + bankNumPerSelector).reduce(_++_)
  })

  for((s, si) <- selectorSeq zip selectorInput){
    s.io.in.zip(si).foreach({case(inPort, driver) =>
      inPort.valid := driver._1._1._1
      inPort.bits.info := driver._1._1._2
      inPort.bits.bankIdxOH := driver._1._2
      inPort.bits.entryIdxOH := driver._2
    })
  }
  for((outPort,driver) <- io.issueInfo zip selectorSeq){
    outPort.valid := driver.io.out.valid
    outPort.bits.bankIdxOH := driver.io.out.bits.bankIdxOH
    outPort.bits.entryIdxOH := driver.io.out.bits.entryIdxOH
    outPort.bits.info := driver.io.out.bits.info
  }

  private val flatInputInfoVec = VecInit(io.selectInfo.map(_.reverse).reverse.reduce(_++_))
  for(outPort <- io.issueInfo){
    val selectedInfo = Mux1H(UIntToOH(OHToUInt(outPort.bits.bankIdxOH) * entryNum.U + OHToUInt(outPort.bits.entryIdxOH)), flatInputInfoVec)
    xs_assert(Mux(outPort.valid, selectedInfo.valid & fuTypeList.map(_ === selectedInfo.bits.fuType).reduce(_|_), true.B))
  }

  //TODO: Should check if the selected instruction is the oldest one
}
