package genes.example.design.GenesExample.pipeline.scalarPipe.sysCall

import chisel3._
import genes.organs.rocket.config.Parameters
import genes.example.design.GenesExample.pipeline._
import genes.example.design.GenesExample.pipeline.scalarPipe._
import genes.example.design.GenesExample.spec.CSRConstants._
import genes.example.design.GenesExample.spec.Causes
import genes.bones._

class SysCall(implicit p: Parameters, pipeline: Pipeline) extends ScalarPipePlugin {
  def isECALL(inst: Instruction): Bool = inst.ir(31, 7) === 0.U
  def isEBREAK(inst: Instruction): Bool = inst.ir(31, 20) === 1.U && inst.ir(19, 7) === 0.U

  def ECALLValid(s: PipeStage): Bool = s.enq.isFire && s.input(Stageables().CTRL).csr === CSR_I && isECALL(s.input(Stageables().IR))
  def EBREAKValid(s: PipeStage): Bool = s.enq.isFire && s.input(Stageables().CTRL).csr === CSR_I && isEBREAK(s.input(Stageables().IR))

  val EcallExceptionClient = Services().ExceptionService(ScalarPipe.Stages("execute"), Causes.ExceptionPriority(Causes.machine_ecall))
  val EBreakExceptionClient = Services().ExceptionService(ScalarPipe.Stages("execute"), Causes.ExceptionPriority(Causes.breakpoint, Causes.BreakType.EevBP))


  def build(): Unit = {
    ScalarPipe.Stages("execute") plug {
      EcallExceptionClient.valid := ECALLValid(thisStage)
      EcallExceptionClient.payload.default.code := Causes.machine_ecall.U
      EBreakExceptionClient.valid := EBREAKValid(thisStage)
      EBreakExceptionClient.payload.default.code := Causes.breakpoint.U
    }
  }
}
