package genes.example.design.GenesExample.pipeline.scalarPipe

import genes.bones._

class ScalarPipe extends MultiStageSingleChannelStage("ScalarPipe") with FiredPipe {
  def subPipe(): Unit = {
    connection {
      AddStage(StdStage("execute")) --> AddStage(RegStage("memAccess")) --> AddStage(RegStage("writeBack"))
    }
  }
}
