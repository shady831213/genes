package genes.example.design.GenesExample.modules.loadStore

import chisel3._
import chisel3.util._
import genes.example.design.GenesExample.config._
import genes.organs.rocket.config.Parameters

class LoadStoreCmdCh(implicit p: Parameters) extends GEBundle {
  val addr = UInt(xlen.W)
  val size = UInt(2.W)
  val wr = Bool()
  val len = UInt(log2Ceil(busMxLen - 1).W)
}

class LoadStoreDataCh(implicit p: Parameters) extends GEBundle {
  val data = UInt(xlen.W)
  val last = Bool()
}

class LoadStoreWDataCh(implicit p: Parameters) extends LoadStoreDataCh {
  val mask = UInt((xlen >> 3).W)
}

class LoadStoreRespCh(implicit p: Parameters) extends GEBundle {
  val error = Bool()
}

class LoadStoreIO(implicit p: Parameters) extends GEBundle {
  val cmd = Decoupled(new LoadStoreCmdCh())
  val wdata = Decoupled(new LoadStoreWDataCh())
  val rdata = Flipped(Decoupled(new LoadStoreDataCh()))
  val resp = Flipped(Decoupled(new LoadStoreRespCh()))
}
