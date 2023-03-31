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

class SelectorInfo(bankIdxWidth:Int, entryIdxWidth:Int) extends Bundle{
  val robPtr = new RobPtr
  val entryIdx = UInt(entryIdxWidth.W)
  val bankIdx = UInt(bankIdxWidth.W)
}

class SelectMux(bankIdxWidth:Int, entryIdxWidth:Int) extends Module{
  val io = IO(new Bundle{
    val in0 = Input(Valid(new SelectorInfo(bankIdxWidth, entryIdxWidth)))
    val in1 = Input(Valid(new SelectorInfo(bankIdxWidth, entryIdxWidth)))
    val out = Output(Valid(new SelectorInfo(bankIdxWidth, entryIdxWidth)))
  })
  private val valid0 = io.in0.valid
  private val valid1 = io.in1.valid
  private val ptr0 = io.in0.bits.robPtr
  private val ptr1 = io.in1.bits.robPtr
  private val validVec = Cat(valid1, valid0)
  private val sel = Mux(validVec === "b01".U, true.B, Mux(validVec === "b10".U, false.B, Mux(validVec === "b11".U, ptr0 < ptr1, true.B)))
  private val res = Mux(sel, io.in0, io.in1)
  io.out := res
}
object SelectMux{
  def apply(in0: Valid[SelectorInfo], in1: Valid[SelectorInfo], bankIdxWidth:Int, entryIdxWidth:Int):Valid[SelectorInfo] = {
    val smux = Module(new SelectMux(bankIdxWidth, entryIdxWidth))
    smux.io.in0 := in0
    smux.io.in1 := in1
    smux.io.out
  }
}

class Selector(bankNum:Int, entryNum:Int) extends Module{
  private val bankIdxWidth = log2Ceil(bankNum)
  private val entryIdxWidth = log2Ceil(entryNum)
  val io = IO(new Bundle{
    val in = Input(Vec(entryNum, Valid(new SelectorInfo(bankIdxWidth, entryIdxWidth))))
    val out = Output(Valid(new SelectorInfo(bankIdxWidth, entryIdxWidth)))
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
  *   issuePtr: [Output][Vec][Valid]
  *     bankIdx:
  *       The bank index of selected instruction.
  *     entryIdx:
  *       The entry index of selected instruction.
  * }}}
*/

class SelectNetwork(bankNum:Int, entryNum:Int, issueNum:Int, fuTypeList:Seq[UInt]) extends Module {
  require(issueNum <= bankNum && 0 < issueNum && bankNum % issueNum == 0, "Illegal number of issue ports are supported now!")
  val io = IO(new Bundle{
    val selectInfo = Input(Vec(bankNum,Vec(entryNum, new SelectInfo)))
    val issuePtr = Output(Vec(issueNum, Valid(new Bundle{
      val bankIdx = UInt(log2Ceil(bankNum).W)
      val entryIdx = UInt(log2Ceil(entryNum).W)
    })))
  })

  private val issueValidBitVecList = io.selectInfo.map(_.map(info => info.readyToIssue & (Cat(fuTypeList.map(_ === info.fuType)).orR)))
  private val issueDataVecList = io.selectInfo
  private val issueBankIdxVecList = io.selectInfo.indices.map(idx => Seq.fill(entryNum)(idx.U))
  private val issueEntryIdxVecList = io.selectInfo.indices.map(idx => Seq.tabulate(entryNum)(_.U))
  private val issueAllDataList = issueValidBitVecList.zip(issueDataVecList).zip(issueBankIdxVecList).zip(issueEntryIdxVecList).map({
    case(((v, d),bi),ei) => v.zip(d).zip(bi).zip(ei)
  })

  private val bankNumPerSelector = bankNum / issueNum
  private val selectorSeq = Seq.tabulate(issueNum){idx => Module(new Selector(bankNum, bankNumPerSelector * entryNum))}

  private val selectorInput = Seq.tabulate(issueNum)({idx =>
    issueAllDataList.slice(idx*bankNumPerSelector, idx*bankNumPerSelector + bankNumPerSelector).reduce(_++_)
  })

  for((s, si) <- selectorSeq zip selectorInput){
    s.io.in.zip(si).foreach({case(inPort, driver) =>
      inPort.valid := driver._1._1._1
      inPort.bits.robPtr := driver._1._1._2.robPtr
      inPort.bits.bankIdx := driver._1._2
      inPort.bits.entryIdx := driver._2
    })
  }
  for((outPort,driver) <- io.issuePtr zip selectorSeq){
    outPort.valid := driver.io.out.valid
    outPort.bits.bankIdx := driver.io.out.bits.bankIdx
    outPort.bits.entryIdx := driver.io.out.bits.entryIdx
  }

  private val flatInputInfoVec = VecInit(io.selectInfo.map(_.reverse).reverse.reduce(_++_))
  for(outPort <- io.issuePtr){
    val selectedInfo = Mux1H(UIntToOH(outPort.bits.bankIdx * entryNum.U + outPort.bits.entryIdx), flatInputInfoVec)
    xs_assert(Mux(outPort.valid, selectedInfo.readyToIssue & fuTypeList.map(_ === selectedInfo.fuType).reduce(_|_), true.B))
  }

  //TODO: Should check if the selected instruction is the oldest one
}
