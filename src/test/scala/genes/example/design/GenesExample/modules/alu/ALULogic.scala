package genes.example.design.GenesExample.modules.alu

import chisel3._
import chisel3.util._
import genes.example.design.GenesExample.config.{GEBundle, GEMultiIOModule}
import genes.example.design.GenesExample.spec.ALUConstants._
import genes.example.design.GenesExample.spec.ScalarOpConstants._
import genes.organs.rocket.config.Parameters


//from rocket
class ALUSigs(len: Int)(implicit p: Parameters) extends GEBundle()(p) {
  val dw = Input(UInt(SZ_DW.W))
  val fn = Input(UInt(SZ_ALU_FN.W))
  val in2 = Input(UInt(len.W))
  val in1 = Input(UInt(len.W))
  val out = Output(UInt(len.W))
  val adder_out = Output(UInt(len.W))
  val cmp_out = Output(Bool())

  override def cloneType: ALUSigs.this.type = new ALUSigs(len).asInstanceOf[ALUSigs.this.type]
}

class ALULogic(len: Int)(implicit p: Parameters) extends GEMultiIOModule()(p) {
  require(len == 32 || len == 64)
  val io = IO(new ALUSigs(len))
  // ADD, SUB
  val in2_inv = Mux(isSub(io.fn), ~io.in2, io.in2)
  val in1_xor_in2 = io.in1 ^ in2_inv.asUInt()
  io.adder_out := io.in1 + in2_inv.asUInt() + isSub(io.fn)

  // SLT, SLTU
  val slt =
    Mux(io.in1(len - 1) === io.in2(len - 1), io.adder_out(len - 1),
      Mux(cmpUnsigned(io.fn), io.in2(len - 1), io.in1(len - 1)))
  io.cmp_out := cmpInverted(io.fn) ^ Mux(cmpEq(io.fn), in1_xor_in2 === 0.U, slt)

  // SLL, SRL, SRA
  val (shamt, shin_r) =
    if (len == 32) (io.in2(4, 0), io.in1)
    else {
      require(len == 64)
      val shin_hi_32 = Fill(32, isSub(io.fn) && io.in1(31))
      val shin_hi = Mux(io.dw === DW_64, io.in1(63, 32), shin_hi_32)
      val shamt = Cat(io.in2(5) & (io.dw === DW_64), io.in2(4, 0))
      (shamt, Cat(shin_hi, io.in1(31, 0)))
    }
  val shin = Mux(io.fn === FN_SR || io.fn === FN_SRA, shin_r, Reverse(shin_r))
  val shout_r = (Cat(isSub(io.fn) & shin(len - 1), shin).asSInt >> shamt) (len - 1, 0)
  val shout_l = Reverse(shout_r)
  val shout = Mux(io.fn === FN_SR || io.fn === FN_SRA, shout_r, 0.U) |
    Mux(io.fn === FN_SL, shout_l, 0.U)

  // AND, OR, XOR
  val logic = Mux(io.fn === FN_XOR || io.fn === FN_OR, in1_xor_in2, 0.U) |
    Mux(io.fn === FN_OR || io.fn === FN_AND, io.in1 & io.in2, 0.U)
  val shift_logic = (isCmp(io.fn) && slt) | logic | shout
  val out = Mux(io.fn === FN_ADD || io.fn === FN_SUB, io.adder_out, shift_logic)

  io.out := out
  if (len > 32) {
    require(len == 64)
    when(io.dw === DW_32) {
      io.out := Cat(Fill(32, out(31)), out(31, 0))
    }
  }
}

