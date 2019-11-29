package genes.example.design.GenesExample.pipeline.regFile

import chisel3._
import genes.example.design.GenesExample.config.GEPlugin
import genes.example.design.GenesExample.pipeline._
import genes.organs.rocket.config.Parameters
import genes.example.design.GenesExample.modules.regFile.RegFileLogic
import genes.backbone._

class RegFile(implicit p: Parameters, pipeline: Pipeline) extends GEPlugin {

  def build(): Unit = {
    val regFile = new RegFileLogic(31, xlen, true)
    val readPorts = Services().RegFileReadPortService.Collect
    val writePorts = Services().RegFileWritePortService.Collect

    def bypass(source: UInt) = {
      Services().RegFileBypassPortService.Collect.foldRight(regFile.read(source))((a, b) => Mux(a.valid && a.payload.addr =/= 0.U && a.payload.addr === source, a.payload.data, b))
    }

    readPorts.foreach(c => c.data := bypass(c.addr))

    Services().RegFileMonitorService.Collect.map(_ ()).foreach { case (regId, data) => data := regFile.read(regId) }

    writePorts.foreach(c => {
      when(c.valid) {
        regFile.write(c.payload.addr, c.payload.data)
      }
    })


  }
}