package genes.example.design.GenesExample.pipeline.scalarPipe.alu

import chisel3._
import chisel3.util._
import genes.organs.rocket.config._
import genes.example.design.GenesExample.modules.alu.{ALULogic, ALUSigs}
import genes.backbone._
import genes.example.design.GenesExample.spec.ScalarOpConstants._
import genes.organs.utils._
import genes.example.design.GenesExample.pipeline._
import genes.example.design.GenesExample.pipeline.scalarPipe._


class ALU(implicit p: Parameters, pipeline: Pipeline) extends ScalarPipePlugin {
  val writeRegClient = Services().WriteRegFileService(ScalarPipe.Stages("execute"))

  def sample(sigs: ALUSigs, stage: PipeStage): Unit = {
    val ctrl = stage.input(Stageables().CTRL)
    sigs.dw := ctrl.alu_dw
    sigs.fn := ctrl.alu_fn
    sigs.in1 := MuxLookup(ctrl.sel_alu1, 0.S, Seq(
      A1_RS1.U -> stage.input(Stageables().RS)(0).asSInt,
      A1_PC.U -> stage.input(Stageables().PC).asSInt)).asUInt
    sigs.in2 := MuxLookup(ctrl.sel_alu2, 0.S, Seq(
      A2_RS2.U -> stage.input(Stageables().RS)(1).asSInt,
      A2_IMM.U -> stage.input(Stageables().IR).imm(ctrl.sel_imm),
      A2_SIZE.U -> 4.S)).asUInt
    //not support rvc
    //A2_SIZE -> Mux(ex_reg_rvc, 2.S, 4.S)))
  }

  val alu = Module(new ALULogic(xlen))

  def build(): Unit = {
    ScalarPipe.Stages("execute") plug {
      sample(alu.io, thisStage)
      asStart(Stageables().ALU_OUT) := alu.io.out
      asStart(Stageables().ALU_ADDEROUT) := alu.io.adder_out
      asStart(Stageables().ALU_CMPOUT) := alu.io.cmp_out
      writeRegClient.valid := enq.isFire
      writeRegClient.payload := output(Stageables().ALU_OUT)
    }
  }
}
