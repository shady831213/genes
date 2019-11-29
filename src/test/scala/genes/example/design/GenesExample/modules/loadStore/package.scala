package genes.example.design.GenesExample.modules

import chisel3._
import chisel3.util._
import genes.example.design.GenesExample.config._
import genes.example.design.GenesExample.pipeline.Instruction
import genes.organs.rocket.config.Parameters


package object loadStore {

  implicit class MemSubCmd(inst: Instruction) {
    def sizeBits: UInt = inst.funct3(1, 0)

    def extBit: Bool = !inst.funct3(2)
  }

  def bitOffset(byteOffset: => UInt): UInt = (byteOffset << 3).asUInt()

  def byteOffset(addr: UInt)(implicit p: Parameters): UInt = addr(log2Ceil(p(XLEN) >> 3) - 1, 0)

  def genMask(size: UInt)(implicit p: Parameters): UInt = MuxLookup(size, 1.U((p(XLEN) >> 3).W).unary_-(), Seq(
    "b00".U -> 1.U((p(XLEN) >> 3).W),
    "b01".U -> 3.U((p(XLEN) >> 3).W),
    "b10".U -> 0xf.U((p(XLEN) >> 3).W),
  ))

  def getHeadMask(addr: UInt, size: UInt)(implicit p: Parameters): UInt = (genMask(size) << byteOffset(addr)).asUInt()

  def getTailMask(elen: UInt, size: UInt)(implicit p: Parameters): UInt = {
    val blen = (elen << (1.U << size).asUInt()).asUInt()
    assert(blen < (p(XLEN) >> 3).asUInt())
    (~(genMask(size) << byteOffset(blen))).asUInt()
  }

  def alignToBoundary(addr: UInt)(implicit p: Parameters) = {
    (~((~addr).asUInt() | ((1 << log2Ceil(p(XLEN) >> 3)).U - 1.U))).asUInt()
  }

}
