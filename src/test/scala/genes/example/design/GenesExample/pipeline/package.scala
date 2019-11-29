package genes.example.design.GenesExample

import chisel3._
import chisel3.util._
import genes.example.design.GenesExample.config._
import genes.example.design.GenesExample.modules.csr.Vtype
import genes.example.design.GenesExample.spec.ALUConstants._
import genes.example.design.GenesExample.spec.CSRConstants._
import genes.example.design.GenesExample.spec.Causes.SZ_CAUSE
import genes.example.design.GenesExample.spec.MemoryOpConstants._
import genes.example.design.GenesExample.spec.ScalarOpConstants._
import genes.organs.rocket.config.Parameters
import genes.organs.rocket.decode.DecodeLogic
import genes.organs.utils._

package object pipeline {

  class IntCtrlSigs extends Bundle {
    val legal = Bool()
    val fp = Bool() //not support
    val rocc = Bool() //not support
    val branch = Bool()
    val jal = Bool()
    val jalr = Bool()
    val rxs2 = Bool()
    val rxs1 = Bool()
    val scie = Bool() //not support
    val sel_alu2 = UInt(SZ_AX.W)
    val sel_alu1 = UInt(SZ_AX.W)
    val sel_imm = UInt(SZ_IMM.W)
    val alu_dw = Bool()
    val alu_fn = UInt(SZ_ALU_FN.W)
    val mem = Bool()
    val mem_cmd = UInt(M_SZ.W)
    val rfs1 = Bool() //not support
    val rfs2 = Bool() //not support
    val rfs3 = Bool() //not support
    val wfd = Bool() //not support
    val mul = Bool() //not support
    val div = Bool() //not support
    val wxd = Bool() //not support
    val csr = UInt(SZ_CSR.W)
    val fence_i = Bool() //not support
    val fence = Bool() //not support
    val amo = Bool() //not support
    val dp = Bool() //not support
    val vec = Bool()
    val setvl = Bool()

    def csr_valid = csr >= CSR_R.U

    def bypassable = !fence_i && !fence && !branch && !jal && !jalr && !mul && !div && !amo && !fp

    def vec2scalar = vec && wxd
  }

  trait DecodeTable extends GEParams {
    val table: Array[(BitPat, List[BitPat])]
  }

  class DecodeSigs extends IntCtrlSigs {
    def default: List[BitPat] =
    //           jal                                                             renf1               fence.i
    //   val     | jalr                                                          | renf2             |
    //   | fp_val| | renx2                                                       | | renf3           |
    //   | | rocc| | | renx1       s_alu1                          mem_val       | | | wfd           |
    //   | | | br| | | |   s_alu2  |       imm    dw     alu       | mem_cmd     | | | | mul         |
    //   | | | | | | | |   |       |       |      |      |         | |           | | | | | div       | fence
    //   | | | | | | | |   |       |       |      |      |         | |           | | | | | | wxd     | | amo            setvl
    //   | | | | | | | | scie      |       |      |      |         | |           | | | | | | |       | | | dp        vec|
      List(N, X, X, X, X, X, X, X, X, A2_X, A1_X, IMM_X, DW_X, FN_X, N, M_X, X, X, X, X, X, X, X, CSR_X, X, X, X, X, X, X)

    def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
      val decoder = DecodeLogic(inst, default, table)
      val sigs = Seq(legal, fp, rocc, branch, jal, jalr, rxs2, rxs1, scie, sel_alu2,
        sel_alu1, sel_imm, alu_dw, alu_fn, mem, mem_cmd,
        rfs1, rfs2, rfs3, wfd, mul, div, wxd, csr, fence_i, fence, amo, dp, vec, setvl)
      sigs zip decoder map { case (s, d) => s := d }
      this
    }
  }

  class Instruction(implicit p: Parameters) extends GEBundle()(p) {
    val ir = UInt(32.W)

    def rs1 = ir(19, 15)

    def rs2 = ir(24, 20)

    def rs3 = ir(31, 27)

    def rd = ir(11, 7)

    def csr = ir(31, 20)

    def privilege = ir(29, 28)

    def opcode = ir(6, 0)

    def funct3 = ir(14, 12)

    //from rocket ImmGen
    def imm(sel: UInt): SInt = {
      val sign = Mux(sel === IMM_Z, 0.S, ir(31).asSInt)
      val b30_20 = Mux(sel === IMM_U, ir(30, 20).asSInt, sign)
      val b19_12 = Mux(sel =/= IMM_U && sel =/= IMM_UJ, sign, ir(19, 12).asSInt)
      val b11 = Mux(sel === IMM_U || sel === IMM_Z, 0.S,
        Mux(sel === IMM_UJ, ir(20).asSInt,
          Mux(sel === IMM_SB, ir(7).asSInt, sign)))
      val b10_5 = Mux(sel === IMM_U || sel === IMM_Z, 0.U, ir(30, 25))
      val b4_1 = Mux(sel === IMM_U, 0.U,
        Mux(sel === IMM_S || sel === IMM_SB, ir(11, 8),
          Mux(sel === IMM_Z, ir(19, 16), ir(24, 21))))
      val b0 = Mux(sel === IMM_S, ir(7),
        Mux(sel === IMM_I, ir(20),
          Mux(sel === IMM_Z, ir(15), 0.U)))

      Cat(sign, b30_20, b19_12, b11, b10_5, b4_1, b0).asSInt
    }
  }


  class ExceptionCause(implicit p: Parameters) extends GEBundle()(p) {
    val code = UInt(SZ_CAUSE.W)
    val badAddr = UInt(xlen.W)

    def default: ExceptionCause = {
      code := 0.U
      badAddr := 0.U
      this
    }

    def assign(code: UInt, badAddr: UInt): ExceptionCause = {
      this.code := code
      this.badAddr := badAddr
      this
    }
  }


  class NextPC(implicit p: Parameters) extends GEBundle()(p) {
    val nextPC = UInt(xlen.W)

    def default: NextPC = {
      nextPC := 0.U
      this
    }

    def assign(nextPC: UInt): NextPC = {
      this.nextPC := nextPC
      this
    }
  }

  class RegisterFilePort(val addrWidth: Int, val dataWidth: Int)(implicit p: Parameters) extends GEBundle {
    val addr = UInt(addrWidth.W)
    val data = UInt(dataWidth.W)

    def default: RegisterFilePort = {
      addr := DontCare
      data := DontCare
      this
    }

    def assign(addr: UInt, data: UInt): RegisterFilePort = {
      this.addr := addr
      this.data := data
      this
    }
  }

}
