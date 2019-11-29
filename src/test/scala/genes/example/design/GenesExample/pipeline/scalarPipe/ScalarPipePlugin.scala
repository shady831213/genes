package genes.example.design.GenesExample.pipeline.scalarPipe

import genes.example.design.GenesExample.config.GEPlugin
import genes.example.design.GenesExample.modules.loadStore.LoadStoreIO
import genes.example.design.GenesExample.pipeline.scalarPipe.alu.ALU
import genes.example.design.GenesExample.pipeline.scalarPipe.branch.BranchAndJump
import genes.example.design.GenesExample.pipeline.scalarPipe.bypass.Bypass
import genes.example.design.GenesExample.pipeline.scalarPipe.csr.CSR
import genes.example.design.GenesExample.pipeline.scalarPipe.exception.Exception
import genes.example.design.GenesExample.pipeline.scalarPipe.fence.Fence
import genes.example.design.GenesExample.pipeline.scalarPipe.hazard.Hazard
import genes.example.design.GenesExample.pipeline.scalarPipe.loadStore.LoadStore
import genes.example.design.GenesExample.pipeline.scalarPipe.writeReg.WriteReg
import genes.example.design.GenesExample.pipeline.scalarPipe.sysCall.SysCall
import genes.bones._
import genes.organs.rocket.config.Parameters

abstract class ScalarPipePlugin(implicit p: Parameters, pipeline: Pipeline) extends GEPlugin {
  def ScalarPipe = pipeline.Stages("ScalarPipe").asInstanceOf[ScalarPipe]

}

object ScalarPipePlugins {
  def apply(loadStoreIO: LoadStoreIO)(implicit p: Parameters, pipeline: Pipeline): Seq[ScalarPipePlugin] = Seq(
    new Bypass,
    new WriteReg,
    new ALU,
    new LoadStore(loadStoreIO),
    new CSR,
    new BranchAndJump,
    new Exception,
    new SysCall,
    new Hazard,
    new Fence
  )
}