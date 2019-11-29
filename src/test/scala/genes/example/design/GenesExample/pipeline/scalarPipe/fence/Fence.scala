package genes.example.design.GenesExample.pipeline.scalarPipe.fence

import chisel3._
import genes.example.design.GenesExample.pipeline.{Services, Stageables}
import genes.example.design.GenesExample.pipeline.scalarPipe.ScalarPipePlugin
import genes.backbone.Pipeline
import genes.organs.rocket.config.Parameters

class Fence(implicit p: Parameters, pipeline: Pipeline) extends ScalarPipePlugin {
  val nextPCClient = Services().NextPCService(ScalarPipe.Stages.head)

  def build(): Unit = {
    ScalarPipe.Stages.head plug {
      //fence_i
      nextPCClient.valid := input(Stageables().CTRL).fence_i && enq.isFire
      nextPCClient.payload.nextPC := input(Stageables().NPC)
      when(input(Stageables().CTRL).fence_i && enq.isFire) {
        enq.flush := true.B
      }
    }
  }
}

