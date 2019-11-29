package genes.example.design.GenesExample.pipeline.scalarPipe.bypass

import genes.example.design.GenesExample.pipeline._
import genes.organs.rocket.config.Parameters
import genes.example.design.GenesExample.pipeline.scalarPipe._
import genes.bones._

class Bypass(implicit p: Parameters, pipeline: Pipeline) extends ScalarPipePlugin {
  val exeBypassPort = Services().RegFileBypassPortService(ScalarPipe.Stages("execute"))
  val memBypassPort = Services().RegFileBypassPortService(ScalarPipe.Stages("memAccess"))


  def build(): Unit = {
    ScalarPipe.Stages("execute") plug {
      exeBypassPort.valid := enq.isFire && input(Stageables().CTRL).wxd && input(Stageables().CTRL).bypassable && !input(Stageables().CTRL).csr_valid && !input(Stageables().CTRL).mem
      exeBypassPort.payload.addr := input(Stageables().IR).rd
      exeBypassPort.payload.data := output(Stageables().REG_WDATA)

    }

    ScalarPipe.Stages("memAccess") plug {
      memBypassPort.valid := enq.isFire && input(Stageables().CTRL).wxd && input(Stageables().CTRL).bypassable && !input(Stageables().CTRL).mem
      memBypassPort.payload.addr := input(Stageables().IR).rd
      memBypassPort.payload.data := output(Stageables().REG_WDATA)
    }
  }
}