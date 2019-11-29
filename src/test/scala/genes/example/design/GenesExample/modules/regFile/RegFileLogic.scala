package genes.example.design.GenesExample.modules.regFile

import chisel3._

import scala.collection.mutable.ArrayBuffer

//from rocket
class RegFileLogic(val n: Int, val w: Int, zero: Boolean = false) {
  val rf = Mem(n, UInt(w.W))
  private val reads = ArrayBuffer[(UInt, UInt)]()
  private var canRead = true

  def read(addr: UInt) = {
    require(canRead)
    reads += addr -> Wire(UInt())
    reads.last._2 := Mux(zero.B && addr === 0.U, 0.U, rf.read(addr))
    reads.last._2
  }

  def write(addr: UInt, data: UInt) = {
    canRead = false
    when(addr =/= 0.U) {
      rf.write(addr, data)
      for ((raddr, rdata) <- reads)
        when(addr === raddr) {
          rdata := data
        }
    }
  }
}

