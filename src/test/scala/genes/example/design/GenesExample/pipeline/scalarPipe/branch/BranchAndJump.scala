package genes.example.design.GenesExample.pipeline.scalarPipe.branch

import genes.example.design.GenesExample.pipeline._
import genes.organs.rocket.config.Parameters
import genes.organs.utils._
import genes.example.design.GenesExample.spec.ScalarOpConstants._
import chisel3._
import genes.example.design.GenesExample.pipeline.scalarPipe._
import genes.backbone._

class BranchAndJump(implicit p: Parameters, pipeline: Pipeline) extends ScalarPipePlugin{
  val nextPCClient = Services().NextPCService(ScalarPipe.Stages("memAccess"))
  val writeRegFileClient = Services().WriteRegFileService(ScalarPipe.Stages("writeBack"))


  def build(): Unit = {
    ScalarPipe.Stages("memAccess") plug {
      val misprediction = WireInit(false.B)
      val target = WireInit(input(Stageables().NPC))
      when(enq.isFire) {
        when(input(Stageables().CTRL).jal) {
          target := (input(Stageables().PC).asSInt + input(Stageables().IR).imm(IMM_UJ.U)).asUInt
          misprediction := target =/= input(Stageables().NPC)
        }
        when(input(Stageables().CTRL).jalr) {
          target := input(Stageables().ALU_OUT)
          misprediction := target =/= input(Stageables().NPC)

        }
        when(input(Stageables().CTRL).branch && input(Stageables().ALU_CMPOUT)) {
          target := (input(Stageables().PC).asSInt + input(Stageables().IR).imm(IMM_SB.U)).asUInt
          misprediction := target =/= input(Stageables().NPC)
        }
      }
      nextPCClient.valid := misprediction
      nextPCClient.payload.nextPC := target
      when(misprediction) {
        enq.flush := true.B
      }
    }

    ScalarPipe.Stages("writeBack") plug {
      writeRegFileClient.valid := input(Stageables().CTRL).jalr && enq.isFire
      writeRegFileClient.payload := input(Stageables().PC) + 4.U
    }
  }
}
