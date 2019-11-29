package genes.example.design.GenesExample.pipeline

import chisel3._
import chisel3.util._
import genes.example.design.GenesExample.config._
import genes.bones._
import genes.organs.rocket.config.Parameters

private[pipeline] class Stageables(implicit val p: Parameters) extends GEParams with WithImplicitWrapper[Parameters] {

  case object ALU_OUT extends Stageable(UInt(xlen.W))

  case object ALU_ADDEROUT extends Stageable(UInt(xlen.W))

  case object ALU_CMPOUT extends Stageable(Bool())


  case object CSR_OUT extends Stageable(UInt(xlen.W))

  case object CSR_WRITE extends Stageable(Bool())

  case object CSR_READ extends Stageable(Bool())

  case object CTRL extends Stageable(new IntCtrlSigs)


  case object IR extends Stageable(new Instruction)

  case object PC extends Stageable(UInt(xlen.W))

  case object NPC extends Stageable(UInt(xlen.W))

  case object RS extends Stageable(Vec(2, UInt(xlen.W)))

  case object REG_WDATA extends Stageable(UInt(xlen.W))

  case object XCPT extends Stageable(ClientIf(new ExceptionCause()))

  case object FETCHERR extends Stageable(Bool())

  //wide pipeline
  case object PCV extends Stageable(Vec(coreWidth, UInt(xlen.W)))

  case object NPCV extends Stageable(Vec(coreWidth, UInt(xlen.W)))

  case object IRV extends Stageable(Vec(coreWidth, new Instruction()))

  case object MASKV extends Stageable(Vec(coreWidth, Bool()))

  case object FETCHERRV extends Stageable(Vec(coreWidth, Bool()))

}

object Stageables extends WithImplicitFactory[Parameters, Stageables] {
  protected def gen(implicit p: Parameters) = new Stageables
}
