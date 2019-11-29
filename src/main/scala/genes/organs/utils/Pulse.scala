package genes.organs.utils

import chisel3._
import chisel3.util.ShiftRegister

object Pulse {
  def apply(in: Bool, n: Int = 1): Bool = {
    require(n >= 0)
    in & !ShiftRegister(in, n)
  }
}

object NegPulse {
  def apply(in: Bool, n: Int = 1): Bool = {
    require(n >= 0)
    !in & ShiftRegister(in, n)
  }
}