package genes.example.design.GenesExample.pipeline

import chisel3._
import chisel3.util._
import genes.organs.rocket.config._
import genes.example.design.GenesExample.config._
import genes.bones._


package object fetch {

  class FetchReq(implicit p: Parameters) extends GEBundle()(p) {
    val pc = UInt(xlen.W)
  }

  class FetchResp(implicit p: Parameters) extends GEBundle()(p) {
    val insts = Vec(fetchWidth, UInt(32.W))
    val error = Bool()
  }

  class FetcherIO(implicit p: Parameters) extends GEBundle()(p) {
    val req = ValidIO(new FetchReq)
    val resp = Flipped(ValidIO(new FetchResp))

    def fire(): Bool = req.valid && resp.valid
  }

  class FetcherMonitorIO(driverIO: FetcherIO)(implicit p: Parameters) extends GEMonitorIO(driverIO)(p) {
    val req = Output(Bool())
    val resp = Output(Bool())

    def fire(): Bool = driverIO.fire()
  }


  // Round address down to the nearest fetch boundary.
  def alignToFetchBoundary(addr: UInt, fetchNum: Int) = {
    (~((~addr).asUInt() | ((fetchNum << 2).U - 1.U))).asUInt()
  }

  def fetchMask(addr: UInt, fetchNum: Int) = {
    // where is the first instruction, aligned to a log(fetchWidth) boundary?
    val idx = if (fetchNum > 1) addr(2 + log2Ceil(fetchNum) - 1, 2) else 0.U
    (((1 << fetchNum) - 1).U << idx) (fetchNum - 1, 0)
  }

  abstract class Fetch(val io: FetcherIO)(implicit p: Parameters, pipeline: Pipeline) extends GEPlugin

}
