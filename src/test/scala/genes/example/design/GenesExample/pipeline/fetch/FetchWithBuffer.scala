package genes.example.design.GenesExample.pipeline.fetch

import chisel3._
import genes.bones._
import genes.example.design.GenesExample.modules.fetch._
import genes.organs.rocket.config.Parameters
import genes.example.design.GenesExample.pipeline._
import genes.example.design.GenesExample.spec.Causes

class FetchWithBuffer(io: FetcherIO)(implicit p: Parameters, pipeline: Pipeline) extends Fetch(io) {
  val fetchBuffer = Module(new FetchBuffer(fetchEntries))

  def build(): Unit = {
    pipeline.Stages("fetch") plug {
      //downstream: dispatch to decoders
      for (i <- 0 until coreWidth) {
        asStart(Stageables().PCV)(i) := fetchBuffer.io.deq.bits.vec(i).pc
        //fixme: no prediction, always pc+4
        asStart(Stageables().NPCV)(i) := fetchBuffer.io.deq.bits.vec(i).pc + 4.U
        asStart(Stageables().MASKV)(i) := fetchBuffer.io.deq.bits.mask(i)
        asStart(Stageables().IRV)(i).ir := fetchBuffer.io.deq.bits.vec(i).inst
        asStart(Stageables().FETCHERRV)(i) := fetchBuffer.io.deq.bits.vec(i).error
      }
      deq.valid := fetchBuffer.io.deq.valid
      fetchBuffer.io.deq.ready := deq.ready
      fetchBuffer.io.flush := deq.flush
      fetchBuffer.io.enq.valid := enq.valid
      enq.ready := fetchBuffer.io.enq.ready

      //upstream: fetch from fetcherIO(wrap fetch)
      io.req.valid := enq.ready && !fetchBuffer.io.flush
      enq.valid := io.resp.valid

      val nextPCR = RegInit("x80000000".U(xlen.W)).suggestName("fetch_nextPC_r")
      when(enq.isFire && !fetchBuffer.io.flush) {
        nextPCR := io.req.bits.pc + (fetchWidth << 2).U
      }
      val nextPCReq = Services().NextPCService.Collect
      val nextPCValid = nextPCReq.valid.suggestName("nextPCValid")
      val nextPC = nextPCReq.payload.nextPC.suggestName("nextPC")
      when(nextPCValid) {
        nextPCR := nextPC
      }
      io.req.bits.pc := alignToFetchBoundary(nextPCR, fetchWidth)

      fetchBuffer.io.enq.bits.pc := io.req.bits.pc
      fetchBuffer.io.enq.bits.insts := io.resp.bits.insts
      fetchBuffer.io.enq.bits.error := io.resp.bits.error
      fetchBuffer.io.enq.bits.mask := fetchMask(nextPCR, fetchWidth).asBools()
    }
  }

}
