package genes.example.design.GenesExample.pipeline.decodePipe.readReg

import genes.example.design.GenesExample.pipeline._
import genes.example.design.GenesExample.pipeline.decodePipe._
import genes.backbone._
import genes.organs.rocket.config.Parameters

class ReadReg(implicit p: Parameters, pipeline: Pipeline) extends DecodePipePlugin {
  val readPorts = (0 until coreWidth).map(i => {
    Seq.fill(2)(Services().RegFileReadPortService())
  }
  )

  def build(): Unit = {
    //load instruction
    for (ch <- 0 until coreWidth) {
      DecodePipe.subStage("regRead", ch) plug {
        readPorts(ch)(0).addr := input(Stageables().IR).rs1
        readPorts(ch)(1).addr := input(Stageables().IR).rs2
        asStart(Stageables().RS)(0) := readPorts(ch)(0).data
        asStart(Stageables().RS)(1) := readPorts(ch)(1).data
      }
    }
  }
}