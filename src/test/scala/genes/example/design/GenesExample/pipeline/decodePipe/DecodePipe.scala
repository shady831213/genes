package genes.example.design.GenesExample.pipeline.decodePipe

import genes.example.design.GenesExample.config._
import genes.backbone._
import genes.organs.rocket.config.Parameters

class DecodePipeChannel(ch: Int) extends MultiStageSingleChannelStage(s"DecodePipeChannel_ch$ch", ch) {
  def subPipe(): Unit = {
    connection {
      AddStage(StdStage("decode")) --> AddStage(StdStage("regRead")) --> AddStage(RegStage("decodeDecouple"))
    }
  }
}

class DecodePipe(implicit val p: Parameters) extends MultiChannelStage("DecodePipe", Some(ch => new DecodePipeChannel(ch)), p(CORE_WIDTH)) with WithDependencyValid {
  def enqRoute(): Unit = {
    require(inputStages.length == 1)
    broadCastInput(inputStages.head, enqs)
  }

  def deqRoute(): Unit = {
    deqs.zipWithIndex.foreach { case (d, id) => {
      connectOutput(d, outputStages(id))
    }
    }
  }
}
