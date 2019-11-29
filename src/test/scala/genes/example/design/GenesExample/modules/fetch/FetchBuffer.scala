package genes.example.design.GenesExample.modules.fetch

import chisel3._
import chisel3.util._
import genes.example.design.GenesExample.config._
import genes.organs.rocket.config._

class FetchVec(implicit p: Parameters) extends GEBundle {
  val pc = UInt(xlen.W)
  val insts = Vec(fetchWidth, UInt(32.W))
  val mask = Vec(fetchWidth, Bool())
  val error = Bool()
}

class PCInstPair(implicit p: Parameters) extends GEBundle {
  val pc = UInt(xlen.W)
  val inst = UInt(32.W)
  val error = Bool()

  def default: PCInstPair = {
    pc := 0.U
    inst := 0.U
    error := 0.U
    this
  }
}

class DecodeVec(implicit p: Parameters) extends GEBundle {
  val vec = Vec(coreWidth, new PCInstPair())
  val mask = Vec(coreWidth, Bool())
}

//from BOOM
class FetchBuffer(numEntries: Int)(implicit p: Parameters) extends GEMultiIOModule {
  require(numEntries > fetchWidth)
  require(numEntries % coreWidth == 0)
  val numRows = numEntries / coreWidth
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(new FetchVec()))
    val deq = new DecoupledIO(new DecodeVec())
    val flush = Input(Bool())
  })

  val ram = Reg(Vec(numEntries, new PCInstPair())).suggestName("fb_uop_ram")
  val deq_vec = Wire(Vec(numRows, Vec(coreWidth, new PCInstPair())))

  val head = RegInit(1.U(numRows.W))
  val tail = RegInit(1.U(numEntries.W))

  val maybe_full = RegInit(false.B)

  //-------------------------------------------------------------
  // **** Enqueue Uops ****
  //-------------------------------------------------------------
  // Step 1: Convert FetchPacket into a vector of MicroOps.
  // Step 2: Generate one-hot write indices.
  // Step 3: Write MicroOps into the RAM.

  def rotateLeft(in: UInt, k: Int) = {
    val n = in.getWidth
    Cat(in(n - k - 1, 0), in(n - 1, n - k))
  }


  val might_hit_head = (1 until fetchWidth).map(k => VecInit(rotateLeft(tail, k).asBools.zipWithIndex.filter { case (e, i) => i % coreWidth == 0 }.
    map { case (e, i) => e }).asUInt).
    map(tail => head & tail).foldLeft(0.U)(_ | _).orR
  val at_head = (VecInit(tail.asBools.zipWithIndex.filter { case (e, i) => i % coreWidth == 0 }
    .map { case (e, i) => e }).asUInt & head).orR
  val do_enq = !(at_head && maybe_full || might_hit_head)

  io.enq.ready := do_enq

  // Input microops.
  val in_mask = Wire(Vec(fetchWidth, Bool()))
  val in_uops = Wire(Vec(fetchWidth, new PCInstPair()))

  // Step 1: Convert FetchPacket into a vector of MicroOps.
  for (i <- 0 until fetchWidth) {
    in_mask(i) := io.enq.valid && io.enq.bits.mask(i)
    in_uops(i).pc := io.enq.bits.pc + (i.U << 2).asUInt()
    in_uops(i).inst := io.enq.bits.insts(i)
    in_uops(i).error := io.enq.bits.error
  }

  // Step 2. Generate one-hot write indices.
  val enq_idxs = Wire(Vec(fetchWidth, UInt(numEntries.W)))

  def inc(ptr: UInt) = {
    val n = ptr.getWidth
    Cat(ptr(n - 2, 0), ptr(n - 1))
  }

  var enq_idx = tail
  for (i <- 0 until fetchWidth) {
    enq_idxs(i) := enq_idx
    enq_idx = Mux(in_mask(i), inc(enq_idx), enq_idx)
  }

  // Step 3: Write MicroOps into the RAM.
  for (i <- 0 until fetchWidth) {
    for (j <- 0 until numEntries) {
      when(do_enq && in_mask(i) && enq_idxs(i)(j)) {
        ram(j) := in_uops(i)
      }
    }
  }

  //-------------------------------------------------------------
  // **** Dequeue Uops ****
  //-------------------------------------------------------------

  val will_hit_tail = (VecInit((0 until numEntries)
    .map(i => if (i % coreWidth == 0) false.B else head(i / coreWidth))).asUInt & tail).orR
  val at_tail = at_head

  val deq_valid = !(at_tail && !maybe_full || will_hit_tail)
  val do_deq = io.deq.ready && deq_valid

  // Generate vec for dequeue read port.


  for (i <- 0 until numEntries) {
    deq_vec(i / coreWidth)(i % coreWidth) := ram(i)
  }

  io.deq.bits.mask.foreach { v => v := deq_valid }
  val mux = Mux1H(head, deq_vec).suggestName("fb_mux")
  io.deq.bits.vec zip mux foreach { case (d, q) => d := q }
  io.deq.valid := deq_valid

  //-------------------------------------------------------------
  // **** Update State ****
  //-------------------------------------------------------------

  when(do_enq) {
    tail := enq_idx
    when(in_mask.reduce(_ || _)) {
      maybe_full := true.B
    }
  }

  when(do_deq) {
    head := inc(head)
    maybe_full := false.B
  }

  when(io.flush) {
    head := 1.U
    tail := 1.U
    maybe_full := false.B
  }
}