package genes.example.design.GenesExample.modules.regFile

import chisel3._
import chisel3.util._
import genes.example.design.GenesExample.config._
import genes.organs.rocket.config._
import genes.organs.wheels.scoreboard.Scoreboard

class RegFileSbCheckCh(regNum: Int, checkWidth: Int)(implicit p: Parameters) extends GEBundle {
  val rs = Vec(checkWidth, Valid(UInt(log2Ceil(regNum).W)))

  override def cloneType: RegFileSbCheckCh.this.type = new RegFileSbCheckCh(regNum, checkWidth).asInstanceOf[RegFileSbCheckCh.this.type]
}

class RegFileSb(regNum: Int, reqWidth: Int, checkWidth: Int, bypassWidth: Int, commitWidth: Int)(implicit p: Parameters) extends GEMultiIOModule {
  val io = IO(new Bundle {
    val req = Flipped(Vec(reqWidth, Valid(UInt(log2Ceil(regNum).W))))
    val check = Flipped(Vec(reqWidth, new RegFileSbCheckCh(regNum, checkWidth)))
    val chechReady = Output(Vec(reqWidth, Bool()))
    val bypass = Flipped(Vec(bypassWidth, Valid(UInt(log2Ceil(regNum).W))))
    val commit = Flipped(Vec(commitWidth, Valid(UInt(log2Ceil(regNum).W))))
    val flush = Input(Bool())
  })

  def isZero(addr: UInt) = addr === 0.U

  def bypassValid(addr: UInt) = (io.bypass ++ io.commit).map { c => c.valid && !isZero(c.bits) && c.bits === addr }.reduce(_ || _)

  //not include r0
  val sb = new Scoreboard(regNum - 1)
  //commit
  sb.clear(VecInit(io.commit.map(c => c.valid && !isZero(c.bits))), VecInit(io.commit.map(_.bits - 1.U)))
  //req
  sb.set(VecInit(io.req.map(r => r.valid && !isZero(r.bits))), VecInit(io.req.map(_.bits - 1.U)))
  //check
  val pipeBusy = io.check.map { c =>
    c.rs.map { r => r.valid && !isZero(r.bits) && sb.read(r.bits - 1.U) && !bypassValid(r.bits) }.reduce(_ || _)
  }.scanLeft(false.B)(_ || _).takeRight(reqWidth)

  val bundleBusy = (io.check.tail zip io.req.reverse.tails.toSeq.tail.reverse.tail).map { case (c, reqs) => {
    reqs.map { req =>
      c.rs.map { r => req.valid && r.valid && !isZero(req.bits) && !isZero(r.bits) && req.bits === r.bits }.reduce(_ || _)
    }.reduce(_ || _)
  }
  }.scanLeft(false.B)(_ || _)
  val busy = pipeBusy zip bundleBusy map { case (p, b) => p || b }
  (io.chechReady zip busy).foreach { case (v, b) => v := !b }
  //flush
  when(io.flush) {
    sb.flush
  }
}
