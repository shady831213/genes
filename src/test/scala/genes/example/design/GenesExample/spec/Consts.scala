package genes.example.design.GenesExample.spec

import chisel3._
import chisel3.util._
import genes.organs.rocket.utils._
import genes.organs.utils._

object ScalarOpConstants {
  val SZ_BR = 3

  val BR_X = BitPat("b???")

  val BR_EQ = 0.U(SZ_BR.W).BP

  val BR_NE = 1.U(SZ_BR.W).BP

  val BR_J = 2.U(SZ_BR.W).BP

  val BR_N = 3.U(SZ_BR.W).BP

  val BR_LT = 4.U(SZ_BR.W).BP

  val BR_GE = 5.U(SZ_BR.W).BP

  val BR_LTU = 6.U(SZ_BR.W).BP

  val BR_GEU = 7.U(SZ_BR.W).BP

  val SZ_AX = 2

  val A1_X = BitPat("b??")

  val A1_ZERO = 0.U(SZ_AX.W).BP

  val A1_RS1 = 1.U(SZ_AX.W).BP

  val A1_PC = 2.U(SZ_AX.W).BP

  val A2_X = BitPat("b??")

  val A2_ZERO = 0.U(SZ_AX.W).BP

  val A2_SIZE = 1.U(SZ_AX.W).BP

  val A2_RS2 = 2.U(SZ_AX.W).BP

  val A2_IMM = 3.U(SZ_AX.W).BP

  val SZ_IMM = 3

  val IMM_X = BitPat("b???")

  val IMM_S = 0.U(SZ_IMM.W).BP

  val IMM_SB = 1.U(SZ_IMM.W).BP

  val IMM_U = 2.U(SZ_IMM.W).BP

  val IMM_UJ = 3.U(SZ_IMM.W).BP

  val IMM_I = 4.U(SZ_IMM.W).BP

  val IMM_Z = 5.U(SZ_IMM.W).BP


  val X = BitPat("b?")

  val N = BitPat("b0")

  val Y = BitPat("b1")

  val SZ_DW = 1

  val DW_X = X

  val DW_32 = false.B.BP

  val DW_64 = true.B.BP

  val DW_XPR = DW_64
}

object MemoryOpConstants {
  val NUM_XA_OPS = 9
  val M_SZ = 5

  val M_X = BitPat("b?????")

  val M_XRD = BitPat("b00000") // int load
  val M_XWR = BitPat("b00001") // int store
  val M_PFR = BitPat("b00010") // prefetch with intent to read
  val M_PFW = BitPat("b00011") // prefetch with intent to write
  val M_XA_SWAP = BitPat("b00100")

  val M_FLUSH_ALL = BitPat("b00101") // flush all lines
  val M_XLR = BitPat("b00110")

  val M_XSC = BitPat("b00111")

  val M_XA_ADD = BitPat("b01000")

  val M_XA_XOR = BitPat("b01001")

  val M_XA_OR = BitPat("b01010")

  val M_XA_AND = BitPat("b01011")

  val M_XA_MIN = BitPat("b01100")

  val M_XA_MAX = BitPat("b01101")

  val M_XA_MINU = BitPat("b01110")

  val M_XA_MAXU = BitPat("b01111")

  val M_FLUSH = BitPat("b10000") // write back dirty data and cede R/W permissions
  val M_PWR = BitPat("b10001") // partial (masked) store
  val M_PRODUCE = BitPat("b10010") // write back dirty data and cede W permissions
  val M_CLEAN = BitPat("b10011") // write back dirty data and retain R/W permissions
  val M_SFENCE = BitPat("b10100") // flush TLB

  def isAMOLogical(cmd: UInt) = cmd.isOneOf(M_XA_SWAP.U, M_XA_XOR.U, M_XA_OR.U, M_XA_AND.U)

  def isAMOArithmetic(cmd: UInt) = cmd.isOneOf(M_XA_ADD.U, M_XA_MIN.U, M_XA_MAX.U, M_XA_MINU.U, M_XA_MAXU.U)

  def isAMO(cmd: UInt) = isAMOLogical(cmd) || isAMOArithmetic(cmd)

  def isPrefetch(cmd: UInt) = cmd === M_PFR || cmd === M_PFW

  def isRead(cmd: UInt) = cmd === M_XRD || cmd === M_XLR || cmd === M_XSC || isAMO(cmd)

  def isWrite(cmd: UInt) = cmd === M_XWR || cmd === M_PWR || cmd === M_XSC || isAMO(cmd)

  def isWriteIntent(cmd: UInt) = isWrite(cmd) || cmd === M_PFW || cmd === M_XLR
}

object ALUConstants {
  val SZ_ALU_FN = 4
  val FN_X = BitPat("b????")
  val FN_ADD = 0.U(SZ_ALU_FN.W).BP
  val FN_SL = 1.U(SZ_ALU_FN.W).BP
  val FN_SEQ = 2.U(SZ_ALU_FN.W).BP
  val FN_SNE = 3.U(SZ_ALU_FN.W).BP
  val FN_XOR = 4.U(SZ_ALU_FN.W).BP
  val FN_SR = 5.U(SZ_ALU_FN.W).BP
  val FN_OR = 6.U(SZ_ALU_FN.W).BP
  val FN_AND = 7.U(SZ_ALU_FN.W).BP
  val FN_SUB = 10.U(SZ_ALU_FN.W).BP
  val FN_SRA = 11.U(SZ_ALU_FN.W).BP
  val FN_SLT = 12.U(SZ_ALU_FN.W).BP
  val FN_SGE = 13.U(SZ_ALU_FN.W).BP
  val FN_SLTU = 14.U(SZ_ALU_FN.W).BP
  val FN_SGEU = 15.U(SZ_ALU_FN.W).BP

  val FN_DIV = FN_XOR
  val FN_DIVU = FN_SR
  val FN_REM = FN_OR
  val FN_REMU = FN_AND

  val FN_MUL = FN_ADD
  val FN_MULH = FN_SL
  val FN_MULHSU = FN_SEQ
  val FN_MULHU = FN_SNE

  def isMulFN(fn: UInt, cmp: UInt) = fn(1, 0) === cmp(1, 0)

  def isSub(cmd: UInt) = cmd(3)

  def isCmp(cmd: UInt) = cmd >= FN_SLT.U

  def cmpUnsigned(cmd: UInt) = cmd(1)

  def cmpInverted(cmd: UInt) = cmd(0)

  def cmpEq(cmd: UInt) = !cmd(3)
}

object RegFileConstants {
  val GP = 3
}

object CSRConstants {
  // commands
  val SZ_CSR = 3
  val SZ_CSR_ADDR = 12
  val CSR_X = BitPat.dontCare(SZ_CSR)

  val CSR_N = 0.U(SZ_CSR.W).BP

  val CSR_R = 2.U(SZ_CSR.W).BP

  val CSR_I = 4.U(SZ_CSR.W).BP

  val CSR_W = 5.U(SZ_CSR.W).BP

  val CSR_S = 6.U(SZ_CSR.W).BP

  val CSR_C = 7.U(SZ_CSR.W).BP

  // mask a CSR cmd with a valid bit
  def maskCmd(valid: Bool, cmd: UInt): UInt = {
    // all commands less than CSR.I are treated by CSRFile as NOPs
    cmd & (~Mux(valid, 0.U, CSR_I.U)).asUInt()
  }

  val ADDRSZ = 12

  def busErrorIntCause = 128

  def debugIntCause = 14 // keep in sync with MIP.debug
  def debugTriggerCause = {
    val res = debugIntCause
    require(!(Causes.all contains res))
    res
  }

  val firstCtr = CSRs.cycle
  val firstCtrH = CSRs.cycleh
  val firstHPC = CSRs.hpmcounter3
  val firstHPCH = CSRs.hpmcounter3h
  val firstHPE = CSRs.mhpmevent3
  val firstMHPC = CSRs.mhpmcounter3
  val firstMHPCH = CSRs.mhpmcounter3h
  val firstHPM = 3
  val nCtr = 32
  val nHPM = nCtr - firstHPM
  val hpmWidth = 40

  val maxPMPs = 16
}