package genes.example.design.GenesExample.pipeline.scalarPipe.hazard

import chisel3._
import genes.organs.rocket.config.Parameters
import genes.example.design.GenesExample.pipeline._
import genes.example.design.GenesExample.pipeline.scalarPipe._
import genes.backbone._

class Hazard(implicit p: Parameters, pipeline: Pipeline) extends ScalarPipePlugin {
  Services().ScalarBusyService(ScalarPipe) := ScalarPipe.Stages.map(_.enq.valid).foldLeft(false.B)(_ || _)

  def build(): Unit = {
  }

}
