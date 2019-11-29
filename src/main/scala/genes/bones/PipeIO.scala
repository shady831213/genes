package genes.bones

import chisel3._
import chisel3.util._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

protected[bones] trait PipeDataBase {
  var pipeName: String = ""
  val datas = mutable.HashMap[Stageable[Data], Data]()

  def data[T <: Data](key: Stageable[T]): T

  protected def target: mutable.HashMap[Stageable[Data], Data]

  protected[bones] def =:=(that: PipeDataBase): Unit = {
    for ((key, stageable) <- target) {
      if (that.datas.get(key).isDefined) {
        stageable := that.data(key)
      }
    }
  }

  protected[bones] def copyDataFrom(that: PipeDataBase): Unit = {
    require(datas.isEmpty)
    for (key <- that.target.keys) {
      data(key)
    }
  }
}

protected[bones] trait PipeData extends PipeDataBase {
  def data[T <: Data](key: Stageable[T]): T = {
    datas.getOrElseUpdate(key.asInstanceOf[Stageable[Data]],
      Wire(key()).suggestName(s"${pipeName}_${key.pipeName}")
    ).asInstanceOf[T]
  }

  protected def target: mutable.HashMap[Stageable[Data], Data] = datas

}

protected[bones] trait PipeDataWithDefault extends PipeDataBase {
  val datasDefault = mutable.HashMap[Stageable[Data], Data]()

  def data[T <: Data](key: Stageable[T]): T = {
    datas.getOrElseUpdate(key.asInstanceOf[Stageable[Data]], {
      val output = Wire(key()).suggestName(s"${pipeName}_${key.pipeName}")
      val outputDefault = Wire(key()).suggestName(s"${pipeName}_${key.pipeName}_default")
      datasDefault(key.asInstanceOf[Stageable[Data]]) = outputDefault
      output := outputDefault
      output
    }).asInstanceOf[T]
  }

  protected def target: mutable.HashMap[Stageable[Data], Data] = datasDefault

}

protected[bones] abstract class PipeControl extends Bundle with PipeDataBase {
  val valid = Bool()
  val ready = Bool()
  val flush = Bool()
  val mask = Bool()

  def isFire = valid && ready

  def default: Unit = {
    valid := false.B
    ready := true.B
    flush := false.B
    mask := true.B
  }

  protected[bones] def =~=(that: PipeControl): Unit = {
    that.flush := flush
    that.ready := ready
    valid := that.valid
    mask := that.mask
  }

  protected[bones] def =|=(that: PipeControl): Unit = {
    this =~= that
    this =:= that
  }

  protected[bones] def =||=(that: PipeControl): Unit = {
    that.flush := flush
    that.ready := ready
    val validReg = RegInit(false.B).suggestName(s"${pipeName}_valid_r")
    when(ready || flush) {
      validReg := false.B
    }
    when(!flush && ready) {
      validReg := that.valid
    }

    valid := validReg

    val maskReg = RegInit(false.B).suggestName(s"${pipeName}_mask_r")
    when(ready) {
      maskReg := that.mask
    }
    mask := maskReg

    for ((key, data) <- target) {
      if (that.datas.get(key).isDefined) {
        val stageableReg = RegInit(that.data(key)).suggestName(s"${pipeName}_${
          key.pipeName
        }")
        when(ready) {
          stageableReg := that.data(key)
        }
        data := stageableReg
      }
    }
  }
}

protected[bones] class PipeIORouter {
  val arbitTable = mutable.HashMap[PipeControl, (Int => ArbiterIO[Bool], Seq[PipeControl])]()
  val connectTable = mutable.HashMap[PipeControl, PipeControl]()
  val reverseSelTable = mutable.HashMap[PipeControl, ArrayBuffer[(Bool, PipeControl)]]()
  val broadCastTable = mutable.HashMap[PipeControl, ArrayBuffer[PipeControl]]()
  val targetSet = mutable.Set[PipeControl]()

  private def checkMultiDirven(target: PipeControl): Unit = {
    require(!targetSet.contains(target), s"${target.pipeName} has multi driven!")
    targetSet += target
  }

  def arbit(sources: Seq[PipeControl], target: PipeControl, arbiterGen: Int => ArbiterIO[Bool]): Unit = {
    require(arbitTable.get(target).isEmpty)
    if (sources.nonEmpty) {
      arbitTable(target) = arbiterGen -> sources
    }
  }

  def connect(source: PipeControl, target: PipeControl): Unit = {
    require(connectTable.get(target).isEmpty)
    connectTable(target) = source
  }

  def broadCast(source: PipeControl, targets: Seq[PipeControl]): Unit = {
    broadCastTable.getOrElseUpdate(source, ArrayBuffer[PipeControl]()) ++= targets
  }

  def route(): Unit = {
    broadCastTable.foreach { case (source, targets) => {
      targets.foreach { target => {
        checkMultiDirven(target)
        target =:= source
        target.valid := source.valid
        target.mask := source.mask
      }
      }
      source.ready := targets.map { target => target.ready }.reduce(_ && _)
      source.flush := targets.map { target => target.flush }.reduce(_ || _)
    }
    }

    arbitTable.foreach { case (target, sources) => {
      checkMultiDirven(target)
      val starts = sources._2
      val arbiter = sources._1(starts.length)
      starts.zipWithIndex.foreach {
        case (start, idx) => {
          arbiter.in(idx).valid := start.valid
          start.ready := arbiter.in(idx).ready
          arbiter.in(idx).bits := false.B
          start.flush := target.flush
        }
      }
      for ((key, data) <- target.datas) {
        data := MuxCase(DontCare, starts.zipWithIndex.filter {
          case (start, idx) => {
            start.datas.get(key).isDefined
          }
        }.map { case (start, idx) => {
          (arbiter.chosen === idx.U) -> start.data(key)
        }
        })
      }
      target.mask := MuxCase(true.B, starts.zipWithIndex.map {
        case (start, idx) => {
          (arbiter.chosen === idx.U) -> start.mask
        }
      })

      target.valid := arbiter.out.valid
      arbiter.out.ready := target.ready
    }
    }


    connectTable.toSeq.foreach { case (target, source) => {
      checkMultiDirven(target)
      target =|= source
    }
    }
  }
}

protected[bones] class PipeIOWithDefault extends PipeControl with PipeDataWithDefault

protected[bones] class PipeIO extends PipeControl with PipeData
