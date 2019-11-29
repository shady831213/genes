package genes.organs.buffers

import chisel3._
import chisel3.util._

class RingBuffer(val size: Int,
                 readyFn: UInt => Bool) {
  val head = RegInit(UInt(log2Up(size).W), 0.U)
  val tail = RegInit(UInt(log2Up(size).W), 0.U)
  val updateHead = Wire(Bool())
  val updateTail = Wire(Bool())
  val nextHead = Wire(UInt(log2Up(size).W))
  val nextTail = Wire(UInt(log2Up(size).W))
  val maybeFull = RegInit(Bool(), false.B)

  def step(ptr: UInt, n: Int): UInt = {
    require(n < size)
    if (n == 0) {
      return ptr
    }
    if (isPow2(size))
      ptr + n.U
    else if (n == 1)
      Mux(ptr === (size - 1).U, 0.U, ptr + 1.U)
    else
      ptr + Mux(ptr < (size - n).U, n.U, -(size - n).U(log2Up(size).W))
  }

  def findFirst(v: Vec[Bool], head: UInt, fn: Int => Bool) = {
    val internal = Wire(Vec(2 * size, Bool()))
    for (i <- 0 until size) {
      internal(i + size) := v(i) && fn(i)
      internal(i) := internal(i + size) && (i.U >= head)
    }
    val prioOh = PriorityEncoderOH(internal)
    val out = Wire(Vec(size, Bool()))
    for (i <- 0 until size) {
      out(i) := prioOh(i) | prioOh(i + size)
    }
    out
  }

  def setHead(n: UInt) = {
    updateHead := true.B
    nextHead := n
  }

  def setTail(n: UInt) = {
    updateTail := true.B
    nextTail := n
  }

  val empty = if (isPow2(size)) {
    Cat(!maybeFull && (head === tail), head - tail)
  } else {
    Mux(maybeFull && (head === tail), 0.U, Mux(head > tail, head - tail, size.U - (tail - head)))
  }

  def ready = readyFn(empty)

  def init = {}

  final def header = {
    updateHead := false.B
    updateTail := false.B
    nextHead := head
    nextTail := tail
    init
  }

  def body = {}

  final def logic = {
    when(updateTail) {
      tail := nextTail
      maybeFull := true.B
    }
    when(updateHead) {
      head := nextHead
      maybeFull := false.B
    }
    body
  }
}
