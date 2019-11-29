package genes.example.design.GenesExample.pipeline.decodePipe.decode

import chisel3._
import genes.example.design.GenesExample.pipeline._
import genes.example.design.GenesExample.pipeline.decodePipe._
import genes.example.design.GenesExample.spec.Causes
import genes.bones._
import genes.organs.rocket.config.Parameters

class Decoder(implicit p: Parameters, pipeline: Pipeline) extends DecodePipePlugin {
  def build(): Unit = {
    //load instruction
    for (ch <- 0 until coreWidth) {
      DecodePipe.subStage("decode", ch) plug {
        asStart(Stageables().PC) := output(Stageables().PCV)(ch)
        asStart(Stageables().NPC) := output(Stageables().NPCV)(ch)
        asStart(Stageables().IR).ir := output(Stageables().IRV)(ch).ir
        asStart(Stageables().FETCHERR) := output(Stageables().FETCHERRV)(ch)


        asStart(Stageables().CTRL) := Wire(new DecodeSigs).decode(input(Stageables().IR).ir, new IDecode().table ++ new FenceIDecode(false).table ++ {
          if (xlen == 64) new I64Decode().table else new I32Decode().table
        }
        )
      }
    }
  }
}
