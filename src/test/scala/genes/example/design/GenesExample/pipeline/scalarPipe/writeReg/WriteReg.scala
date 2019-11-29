package genes.example.design.GenesExample.pipeline.scalarPipe.writeReg

import chisel3._
import genes.example.design.GenesExample.pipeline._
import genes.organs.rocket.config.Parameters
import genes.example.design.GenesExample.modules.regFile.RegFileLogic
import genes.example.design.GenesExample.pipeline.scalarPipe._
import genes.bones._

class WriteReg(implicit p: Parameters, pipeline: Pipeline) extends ScalarPipePlugin {
  val writePort = Services().RegFileWritePortService()
  def build(): Unit = {
    var StageCnt = 0
    Services().WriteRegFileService.Collect.foreach { case (stage, logic) => {
      stage plug {
        if (StageCnt == 0) {
          asStart(Stageables().REG_WDATA) := logic.payload
        } else {
          output(Stageables().REG_WDATA) := input(Stageables().REG_WDATA)
          when(logic.valid) {
            output(Stageables().REG_WDATA) := logic.payload
          }
        }
      }
      StageCnt += 1
    }
    }

    ScalarPipe.Stages("writeBack") plug {
      writePort.valid := input(Stageables().CTRL).wxd && input(Stageables().IR).rd =/= 0.U && enq.isFire && !output(Stageables().XCPT).valid
      writePort.payload.addr := input(Stageables().IR).rd
      writePort.payload.data := output(Stageables().REG_WDATA)
    }
  }
}