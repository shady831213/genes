package genes.example.design.GenesExample.pipeline.decodePipe.hazard

import java.security.Provider.Service

import chisel3._
import genes.example.design.GenesExample.modules.csr.Vtype
import genes.example.design.GenesExample.modules.regFile.RegFileSb
import genes.bones._
import genes.organs.rocket.config.Parameters
import genes.example.design.GenesExample.pipeline.decodePipe._
import genes.example.design.GenesExample.pipeline._
import genes.example.design.GenesExample.spec.CSRs
import genes.example.design.GenesExample.spec.ScalarOpConstants.IMM_I
import genes.organs.utils._

class Hazard(implicit p: Parameters, pipeline: Pipeline) extends DecodePipePlugin {

  def build(): Unit = {
    DecodePipe plug {
      (0 until coreWidth).foreach { i => {
        enqs(i).mask := input(Stageables().MASKV)(i)
      }
      }
      //hazards in decode
      val decodeBundleBusy = (0 until coreWidth).map { i => {
        DecodePipe.subStage("decode", i).enq.valid
      }
      }.scanLeft(false.B)(_ || _)
      val decodeScalarBundleBusy = (0 until coreWidth).map { i => {
        DecodePipe.subStage("decode", i).enq.valid && !DecodePipe.subStage("decode", i).input(Stageables().CTRL).vec
      }
      }.scanLeft(false.B)(_ || _)
      val bypassPorts = Services().RegFileBypassPortService.Collect
      val writePorts = Services().RegFileWritePortService.Collect
      val sb = Module(new RegFileSb(32, coreWidth, 2, bypassPorts.length, writePorts.length))
      ((bypassPorts ++ writePorts) zip (sb.io.bypass ++ sb.io.commit)).foreach {
        case (client, sbport) => {
          sbport.valid := client.valid
          sbport.bits := client.payload.addr
        }
      }
      (0 until coreWidth).foreach { i => {
        DecodePipe.subStage("decode", i) plug {
          //fence_i && fence hazard
          when((input(Stageables().CTRL).fence_i || input(Stageables().CTRL).fence)
            && (Services().ScalarBusyService.Listen || decodeBundleBusy(i))) {
            enq.ready := false.B
          }

          //RAW hazard
          sb.io.check(i).rs(0).valid := input(Stageables().CTRL).rxs1
          sb.io.check(i).rs(0).bits := input(Stageables().IR).rs1
          sb.io.check(i).rs(1).valid := input(Stageables().CTRL).rxs2
          sb.io.check(i).rs(1).bits := input(Stageables().IR).rs2
          when(!sb.io.chechReady(i)) {
            enq.ready := false.B
          }
          sb.io.flush := deq.flush
          sb.io.req(i).valid := enq.isFire && input(Stageables().CTRL).wxd
          sb.io.req(i).bits := input(Stageables().IR).rd
        }
      }
      }

      (0 until coreWidth).foreach { i => {

        DecodePipe.subStage("decodeDecouple", i) plug {
          //flush
          when(Services().DecodeFlushService.Collect) {
            enq.flush := true.B
          }
        }
      }
      }
    }
  }

}
