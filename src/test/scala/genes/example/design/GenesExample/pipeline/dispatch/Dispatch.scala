package genes.example.design.GenesExample.pipeline.dispatch

import chisel3._
import genes.example.design.GenesExample.config.GEPlugin
import genes.bones.Pipeline
import genes.organs.rocket.config.Parameters
import genes.example.design.GenesExample.pipeline._

class Dispatch(implicit p: Parameters, pipeline: Pipeline) extends GEPlugin {
  def build(): Unit = {
    for (ch <- 0 until coreWidth) {
      pipeline.Stages(s"dispatch$ch") plug {
        val scalarNotVector = !input(Stageables().CTRL).vec || input(Stageables().FETCHERR) || !input(Stageables().CTRL).legal
        when(scalarNotVector) {
          deqs(0).valid := enqs(0).valid
          enqs(0).ready := deqs(0).ready
          deqs(1).valid := false.B
          enqs(1).ready := true.B
        }.otherwise {
          deqs(1).valid := enqs(1).valid
          enqs(1).ready := deqs(1).ready
          deqs(0).valid := false.B
          enqs(0).ready := true.B
        }
      }
    }
  }
}