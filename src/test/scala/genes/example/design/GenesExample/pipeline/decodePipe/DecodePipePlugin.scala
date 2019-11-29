package genes.example.design.GenesExample.pipeline.decodePipe

import genes.example.design.GenesExample.config.GEPlugin
import genes.example.design.GenesExample.pipeline.decodePipe.decode.Decoder
import genes.example.design.GenesExample.pipeline.decodePipe.hazard.Hazard
import genes.example.design.GenesExample.pipeline.decodePipe.readReg.ReadReg
import genes.backbone.Pipeline
import genes.organs.rocket.config.Parameters

abstract class DecodePipePlugin(implicit p: Parameters, pipeline: Pipeline) extends GEPlugin {
  def DecodePipe = pipeline.Stages("DecodePipe").asInstanceOf[DecodePipe]
}

object DecodePipePlugins {
  def apply()(implicit p: Parameters, pipeline: Pipeline): Seq[DecodePipePlugin] = Seq(
    new Decoder(),
    new ReadReg(),
    new Hazard()
  )
}