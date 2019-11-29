package genes.example.design.GenesExample.pipeline.dispatch

import genes.bones._

class DispatchStage(pipeName: String) extends MultiChannelStdStage(pipeName, 2) {
  def enqRoute(): Unit = {
    require(inputStages.length == 1)
    broadCastInput(inputStages.head, enqs)
  }

  def deqRoute(): Unit = {
    val outputStageMap = outputStages.map(s => s.pipeName -> s).toMap
    connectOutput(deqs.head, outputStageMap("ScalarPipe"))
    connectOutput(deqs(1), outputStageMap("VectorPipe"))
  }
}

object DispatchStages {
  def apply(n: Int): Seq[DispatchStage] = (0 until n).map(i => new DispatchStage(s"dispatch$i"))
}
