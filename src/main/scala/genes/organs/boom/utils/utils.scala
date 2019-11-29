package genes.organs.boom.utils

import genes.organs.rocket.utils.Str
import chisel3._
import chisel3.util._

object BoolToChar {
  /**
    * Take in a Chisel Bool and convert it into a Str
    * based on the Chars given
    *
    * @param c_bool    Chisel Bool
    * @param trueChar  Scala Char if bool is true
    * @param falseChar Scala Char if bool is false
    * @return UInt ASCII Char for "trueChar" or "falseChar"
    */
  def apply(c_bool: Bool, trueChar: Char, falseChar: Char = '-'): UInt = {
    Mux(c_bool, Str(trueChar), Str(falseChar))
  }
}


/**
  * Object to return the lowest bit position after the head.
  */
object AgePriorityEncoder
{
  def apply(in: Seq[Bool], head: UInt): UInt = {
    val n = in.size
    val width = log2Ceil(in.size)
    val n_padded = 1 << width
    val temp_vec = (0 until n_padded).map(i => if (i < n) in(i) && i.U >= head else false.B) ++ in
    val idx = PriorityEncoder(temp_vec)
    idx(width-1, 0) //discard msb
  }
}

object MaskLower {
  def apply(in: UInt) = (0 until in.getWidth).map(i => (in >> i).asUInt()).reduce(_ | _)
}

object MaskUpper {
  def apply(in: UInt) = (0 until in.getWidth).map(i => (in << i).asUInt()).reduce(_ | _)
}

/**
  * N-wide one-hot priority encoder.
  */
object SelectFirstN
{
  def apply(in: UInt, n: Int) = {
    val counts = in.asBools.scanLeft(1.U(n.W))((cnt, elt) => Mux(elt, (cnt << 1).asUInt(), cnt))
    val sels = (0 until n).map(j => VecInit((0 until in.getWidth).map(i => counts(i)(j) & in(i))).asUInt)
    VecInit(sels)
  }
}

object IsOlder {
  def apply(i0: UInt, i1: UInt, head: UInt) = ((i0 < i1) ^ (i0 < head) ^ (i1 < head))
}

/**
  * Object to increment an input value, wrapping it if
  * necessary.
  */
object WrapInc {
  // "n" is the number of increments, so we wrap at n-1.
  def apply(value: UInt, n: Int): UInt = {
    if (isPow2(n)) {
      (value + 1.U) (log2Ceil(n) - 1, 0)
    } else {
      val wrap = (value === (n - 1).U)
      Mux(wrap, 0.U, value + 1.U)
    }
  }
}

/**
  * Object to decrement an input value, wrapping it if
  * necessary.
  */
object WrapDec {
  // "n" is the number of increments, so we wrap at n-1.
  def apply(value: UInt, n: Int): UInt = {
    if (isPow2(n)) {
      (value - 1.U) (log2Ceil(n) - 1, 0)
    } else {
      val wrap = (value === 0.U)
      Mux(wrap, (n - 1).U, value - 1.U)
    }
  }
}

/**
  * Object to sext a value to a particular length.
  */
object Sext
{
  def apply(x: UInt, length: Int): UInt = {
    if (x.getWidth == length) return x
    else return Cat(Fill(length-x.getWidth, x(x.getWidth-1)), x)
  }
}