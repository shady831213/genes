package genes.organs.scoreboard

import chisel3._


class Scoreboard2DEntry(rowNum: Int,
                        colNum: Int) {
  val next = Wire(Vec(rowNum, Vec(colNum, Bool())))
  val sb = Seq.fill(rowNum)(new Scoreboard(colNum))

  def setNext(n: UInt, o: UInt) = {
    this.next(n)(o) := true.B
  }

  def clearNext(n: UInt, o: UInt) = {
    this.next(n)(o) := false.B
  }

  def read(n: UInt) = VecInit(sb.map(_.readAll))(n)

}

class ScoreBoard2D(sbNum: Int,
                   rowNum: Int,
                   colNum: Int) {
  private val nextUpdate = Wire(Vec(rowNum, Bool()))
  protected val sbs = Seq.fill(sbNum)(new Scoreboard2DEntry(rowNum, colNum))

  def update(n: UInt) = nextUpdate(n) := true.B

  def header = {
    for (i <- 0 until rowNum) {
      nextUpdate(i) := false.B
      for (j <- 0 until colNum) {
        sbs.foreach(sb => sb.next(i)(j) := sb.sb(i).read(j.U))
      }
    }
  }

  def logic = {
    for (i <- 0 until rowNum) {
      sbs.foreach(sb => sb.sb(i).update(nextUpdate(i), sb.next(i).asUInt()))
    }
  }
}
