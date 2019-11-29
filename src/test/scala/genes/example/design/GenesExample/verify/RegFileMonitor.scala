package genes.example.design.GenesExample.verify

import chisel3._
import genes.example.design.GenesExample.config.{GEBundle, GEPlugin}
import genes.example.design.GenesExample.pipeline._
import genes.organs.rocket.config.Parameters
import genes.example.design.GenesExample.spec.RegFileConstants._
import genes.backbone._

class RegFileMonitorIO(implicit p: Parameters) extends GEBundle()(p) {
  val gp = UInt(xlen.W)

  def default: RegFileMonitorIO = {
    gp := 0.U
    this
  }
}

class RegFileMonitor(implicit p: Parameters, pipeline: Pipeline) extends GEPlugin {
  val io = Wire(new RegFileMonitorIO).default
  Services().RegFileMonitorService(GP.U->io.gp)

  def build(): Unit = {

  }
}
