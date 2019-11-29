package genes.example.design.GenesExample

import genes.example.design.GenesExample.config._
import genes.example.design.GenesExample.modules.loadStore._
import genes.example.design.GenesExample.pipeline._
import genes.example.design.GenesExample.pipeline.decodePipe.{DecodePipe, DecodePipePlugins}
import genes.example.design.GenesExample.pipeline.dispatch.{Dispatch, DispatchStages}
import genes.example.design.GenesExample.pipeline.fetch._
import genes.example.design.GenesExample.pipeline.regFile.RegFile
import genes.example.design.GenesExample.pipeline.scalarPipe._
import genes.example.design.GenesExample.verify._
import genes.backbone._
import genes.organs.rocket.config.Parameters
import chisel3._

class GenesExample(implicit p: Parameters) extends GEMultiIOModule()(p) with Pipeline {
  type T = GenesExample
  val supportException: Boolean = true
  val io = IO(new Bundle {
    val fetchIO = new FetcherIO
    val loadStoreIO = new LoadStoreIO
  })
  val scalarPipe = AddStage(new ScalarPipe).asInstanceOf[ScalarPipe]
  val decodePipe = AddStage(new DecodePipe).asInstanceOf[DecodePipe]
  val dispatchStages = DispatchStages(coreWidth).map(AddStage(_))
  val vectorPipe = AddStage(StdStage("VectorPipe"))
  connection {
    AddStage(StdStage("fetch")) --> decodePipe --> dispatchStages --> Seq(scalarPipe, vectorPipe)
  }
  val regFileMonitor = new RegFileMonitor()

  def allPlugins(): Seq[Plugin] = new FetchWithBuffer(io.fetchIO) ::
    new RegFile ::
    regFileMonitor ::
    new Dispatch ::
    Nil ++
      DecodePipePlugins() ++
      ScalarPipePlugins(io.loadStoreIO)

  AddPlugin(allPlugins(): _*)
  build()
}

class GenesExampleForTest(implicit p: Parameters) extends GenesExample()(p) {
  val testIO = IO(new Bundle {
    val decodeIR = Output(Stageables().IR apply())
    val decodePC = Output(UInt(xlen.W))
    val regFile = Output(new RegFileMonitorIO())
  })
  testIO.decodePC := scalarPipe.Stages.head.input(Stageables().PC)
  testIO.decodeIR := scalarPipe.Stages.head.input(Stageables().IR)
  testIO.regFile := regFileMonitor.io
}
