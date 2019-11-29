package genes.bones

import chisel3._
import chisel3.util.{Arbiter, ArbiterIO}

import scala.collection.mutable


abstract class PipeStage(val chId: Int) extends PipelineNameable {
  val enqs: Seq[PipeIO]

  val deqs: Seq[PipeIOWithDefault]

  def enq = enqs.head

  def deq = deqs.head


  def getEnq(stage: PipeStage): PipeIO = {
    val edges = this.fanins.filter(_.start == stage)
    require(edges.nonEmpty, s"no connection from stage ${stage.pipeName} to stage $pipeName!")
    edges.head.deq
  }

  def getDeq(stage: PipeStage): PipeIO = {
    val edges = this.fanouts.filter(_.target == stage)
    require(edges.nonEmpty, s"no connection from stage $pipeName to stage ${stage.pipeName}!")
    edges.head.enq
  }

  protected[bones] var fanins = mutable.ListBuffer[PipeEdge]()
  protected[bones] var fanouts = mutable.ListBuffer[PipeEdge]()
  protected[bones] var parent: PipeDAG = null.asInstanceOf[PipeDAG]
  protected[bones] var id: Int = 0

  def index: Int = id

  def outputStages: Seq[PipeStage] = fanouts.map { t => t.target }

  def inputStages: Seq[PipeStage] = fanins.map { t => t.start }

  def isHead: Boolean = inputStages.isEmpty

  def isTail: Boolean = outputStages.isEmpty

  def prevStages: Seq[PipeStage] = {
    val stages = mutable.ListBuffer[PipeStage]()
    dfs(this, stage => {
      if (stage != this) {
        stages += stage
      }
      false
    }, true)
    stages
  }

  def nextStages: Seq[PipeStage] = {
    val stages = mutable.ListBuffer[PipeStage]()
    dfs(this, stage => {
      if (stage != this) {
        stages += stage
      }
      false
    })
    stages
  }

  def otherStages: Seq[PipeStage] = prevStages ++ nextStages


  protected val inputRouter = new PipeIORouter
  protected val outputRouter = new PipeIORouter

  protected def arbitOutput(deqs: Seq[PipeIOWithDefault], stage: PipeStage, deqArbiterGen: Option[Int => ArbiterIO[Bool]] = None): Unit = {
    outputRouter.arbit(deqs, getDeq(stage), deqArbiterGen.getOrElse((n: Int) => Module(new Arbiter(Bool(), n)).suggestName(s"${pipeName}_deqArbiter").io))
  }

  protected def connectOutput(deq: PipeIOWithDefault, stage: PipeStage): Unit = {
    outputRouter.connect(deq, getDeq(stage))
  }


  protected def broadCastOutput(deq: PipeIOWithDefault, stages: Seq[PipeStage]): Unit = {
    outputRouter.broadCast(deq, stages.map { stage => getDeq(stage) })
  }


  protected def arbitInput(Stages: Seq[PipeStage], enq: PipeIO, enqArbiterGen: Option[Int => ArbiterIO[Bool]] = None): Unit = {
    inputRouter.arbit(Stages.map { stage => getEnq(stage) }, enq, enqArbiterGen.getOrElse((n: Int) => Module(new Arbiter(Bool(), n)).suggestName(s"${pipeName}_enqArbiter").io))
  }

  protected def connectInput(stage: PipeStage, enq: PipeIO): Unit = {
    inputRouter.connect(getEnq(stage), enq)
  }

  protected def broadCastInput(stage: PipeStage, targets: Seq[PipeIO]): Unit = {
    inputRouter.broadCast(getEnq(stage), targets)
  }

  protected[bones] val starts = mutable.HashMap[Stageable[Data], Data]()

  protected[bones] def start[T <: Data](key: Stageable[T]): T = {
    starts.getOrElseUpdate(key.asInstanceOf[Stageable[Data]],
      Wire(key()).suggestName(s"${pipeName}_${key.pipeName}_start")
    ).asInstanceOf[T]
  }

  def asStart[T <: Data](key: Stageable[T]): T = {
    start(key)
  }

  def input[T <: Data](key: Stageable[T], ch: Int = 0): T = {
    enqs(ch).data(key)
  }

  def output[T <: Data](key: Stageable[T], ch: Int = 0): T = {
    deqs(ch).data(key)
  }


  def enqRoute(): Unit

  def deqRoute(): Unit

  protected[bones] def connectEnq(): Unit = {
    enqRoute()
    inputRouter.route()
    for ((key, stageable) <- starts) {
      enqs.foreach(_.data(key) := stageable)
    }
  }

  protected[bones] def connectDeq(): Unit = {
    deqRoute()
    outputRouter.route()
  }
}


trait WithReg extends PipeStage {
  private def addPreEnq(enq: PipeIO): PipeIO = {
    val prevEnq = Wire(new PipeIO)
    prevEnq.pipeName = s"${pipeName}_prevInput"
    prevEnq.default
    prevEnq.copyDataFrom(enq)
    prevEnq
  }

  final override protected def arbitInput(Stages: Seq[PipeStage], enq: PipeIO, enqArbiterGen: Option[Int => ArbiterIO[Bool]]): Unit = {
    val prevEnq = addPreEnq(enq)
    super.arbitInput(inputStages, prevEnq, enqArbiterGen)
    enq =||= prevEnq
  }

  final override protected def broadCastInput(stage: PipeStage, targets: Seq[PipeIO]): Unit = {
    val prevEnq = addPreEnq(getEnq(stage))
    prevEnq =||= getEnq(stage)
    inputRouter.broadCast(prevEnq, targets)
  }

  final override protected def connectInput(stage: PipeStage, enq: PipeIO): Unit = {
    val prevEnq = addPreEnq(getEnq(stage))
    prevEnq =||= getEnq(stage)
    inputRouter.connect(prevEnq, enq)
  }
}

//only defaultEnq -> defaultDeq channel
abstract class SingleChannelStage(override val pipeName: String, chId: Int) extends PipeStage(chId) {
  def connectOutput(stage: PipeStage): Unit = {
    connectOutput(deq, stage)
  }

  def broadCastOutput(stages: Seq[PipeStage]): Unit = super.broadCastOutput(deq, stages)

  def enqRoute(): Unit = {
    if (!isHead) {
      if (inputStages.length == 1) {
        connectInput(inputStages.head, enq)
      } else {
        arbitInput(inputStages, enq)
      }
    }
  }

  def deqRoute(): Unit = {
    if (fanouts.nonEmpty) {
      //default
      fanouts.head.enq =~= deq
    }
    fanouts.foreach {
      //default
      e => {
        e.enq =:= deq
      }
    }
  }
}


abstract class SingleStageSingleChannelStage(pipeName: String, chId: Int = 0) extends SingleChannelStage(pipeName, chId) {
  val enqs = Seq(Wire(new PipeIO).suggestName(s"${pipeName}_enq"))
  val deqs = Seq(Wire(new PipeIOWithDefault).suggestName(s"${pipeName}_deq"))
  enq.pipeName = s"${pipeName}_input"
  enq.default
  deq.pipeName = s"${pipeName}_output"
  deq.default
  deq =~= enq

  override def deqRoute(): Unit = {
    super.deqRoute()
    deq =:= enq
  }
}


sealed trait StageWithSubDAG extends PipeDAG {
  protected def subPipe(): Unit

  subPipe()
}

abstract class MultiStageSingleChannelStage(pipeName: String,
                                            chId: Int = 0,
                                            pipe: Boolean = true) extends SingleChannelStage(pipeName, chId) with StageWithSubDAG {


  def Stages = stages

  def Stages(name: String) = stages(name)

  val enqs = stages.head.enqs

  val deqs = Stages.last.deqs

  if (!pipe) {
    val process = RegInit(false.B).suggestName(s"${pipeName}_process")
    when(enq.valid) {
      process := true.B
    }
    when(deq.isFire) {
      process := false.B
    }
    when(process) {
      stages.head.deq.valid := false.B
      enq.ready := false.B
    }
  }
}


trait FiredPipe extends PipeDAG {
  stages.foreach { n => n.deq.valid := n.enq.valid && n.enq.ready }
}


abstract class MultiChannelStage(override val pipeName: String, gen: Option[Int => SingleChannelStage], val channelNum: Int = 1, chId: Int = 0) extends PipeStage(chId) with StageWithSubDAG {
  require(channelNum > 0)

  final protected def subPipe(): Unit = {
    (0 until channelNum).map { id => {
      val stage = new MultichannelSubStage(pipeName, s"MultichannelSubStage$id", id, gen)
      AddStage(stage)
    }
    }
  }

  val enqs = stages.map(_.enq)

  val deqs = stages.map(_.deq)

  def Stages = stages.map(_.asInstanceOf[MultichannelSubStage].stages.last)

  def Stages(name: String) = {
    val map = Stages.map(s => s.pipeName -> s).toMap
    map(name)
  }

  def subStage(name: String, ch: Int) = stages(ch).asInstanceOf[MultichannelSubStage].stages.last match {
    case n: SingleStageSingleChannelStage => n
    case n: MultiStageSingleChannelStage => {
      if (name == n.pipeName) n else n.stages(name)
    }
  }
}

sealed trait WithDependency extends MultiChannelStage {
  val stallsTable = mutable.HashMap[String, Seq[Bool]]()

  stallsTable(dummyStageMagicName) = (0 until channelNum).map(i => enqs(i).valid && !subStage(ColStageNames.head, i).enq.ready).scanLeft(false.B)((s, h) => s || h)
  ColStageNames.foreach(n => {
    stallsTable(n) = (0 until channelNum).map(i => subStage(n, i)).map(n => n.enq.valid && !n.deq.ready).scanLeft(false.B)((s, h) => s || h)
  }
  )

  DecoupleHead()


  private def decouple(key: String, useFire: Boolean): Unit = {
    val finishedMask = RegInit(0.U.asTypeOf(Vec(channelNum, Bool()))).suggestName(s"${key}_finishedMask")
    (0 until channelNum).foreach { i => {
      val stage = {
        if (key == dummyStageMagicName) stages(i).asInstanceOf[MultichannelSubStage].stages.head else subStage(key, i)
      }
      when(stage.enq.ready || stage.enq.flush) {
        finishedMask(i) := false.B
      }.otherwise {
        finishedMask(i) := stage.deq.isFire | finishedMask(i)
      }
      stage.deq.mask := stage.enq.mask && !finishedMask(i)
      if (useFire) {
        stage.deq.valid := stage.enq.valid && stage.deq.mask && !stallsTable(key)(i + 1)
      } else {
        stage.deq.valid := stage.enq.valid && stage.deq.mask && !stallsTable(key)(i)
      }
      stage.enq.ready := !stallsTable(key).last
    }
    }
  }


  private def DecoupleHead(): Unit = decouple(dummyStageMagicName, true)

  protected def Decouple(stageName: String, useFire: Boolean): Unit = decouple(stageName, useFire)

  def ColStageNames = stages.head.asInstanceOf[MultichannelSubStage].stages.last match {
    case n: SingleStageSingleChannelStage => Seq(n.pipeName)
    case n: MultiStageSingleChannelStage => n.stages.map(_.pipeName)
  }
}

trait WithDependencyFired extends WithDependency {
  ColStageNames.map(subStage(_, 0)).filter(n => n.isInstanceOf[WithReg]).map(_.pipeName).foreach(Decouple(_, true))
}


trait WithDependencyValid extends WithDependency {
  ColStageNames.map(subStage(_, 0)).filter(n => n.isInstanceOf[WithReg]).map(_.pipeName).foreach(Decouple(_, false))
}


class StdStage(pipeName: String, chId: Int) extends SingleStageSingleChannelStage(pipeName, chId)

protected[bones] class dummyStage(chId: Int) extends StdStage(dummyStageMagicName, chId)

protected[bones] class MultichannelSubStage(parentPipeName: String, pipeName: String, chId: Int, gen: Option[Int => SingleChannelStage]) extends MultiStageSingleChannelStage(pipeName) {
  def subPipe(): Unit = {
    gen match {
      case Some(g) => {
        require(g(chId).chId == chId, s"gen function of $parentPipeName must set chId of stage!")
        connection(AddStage(new dummyStage(chId)) --> AddStage(g(chId)))
      }
      case None => AddStage(new dummyStage(chId))
    }
  }
}

class RegStage(pipeName: String, chId: Int) extends SingleStageSingleChannelStage(pipeName, chId) with WithReg

class MultiCycleStage(pipeName: String,
                      val length: Int,
                      pipe: Boolean = true,
                      chId: Int) extends MultiStageSingleChannelStage(pipeName, chId, pipe) {
  protected def subPipe(): Unit = {
    AddStage(StdStage(s"${pipeName}_0"))
    for (i <- 1 to length) {
      //if sub stage add new stageable use asStart, parent should also add it
      AddStage(RegStage(s"${pipeName}_$i"))
    }
    val sortedStages = stages.sortWith(_.pipeName < _.pipeName)
    connection {
      sortedStages.reduce(_ --> _)
    }
  }
}

abstract class MultiStageSingleChannelRegStage(pipeName: String,
                                               chId: Int = 0,
                                               pipe: Boolean = true) extends MultiStageSingleChannelStage(pipeName, chId, pipe) with WithReg

abstract class MultiChannelStdStage(pipeName: String, channelNum: Int = 1, chId: Int = 0) extends MultiChannelStage(pipeName, None, channelNum, chId)

abstract class MultiChannelRegStage(pipeName: String, channelNum: Int = 1, chId: Int = 0) extends MultiChannelStdStage(pipeName, channelNum, chId) with WithReg


object StdStage {
  def apply(): StdStage = apply("StdStage")

  def apply(name: String): StdStage = {
    apply(name, 0)
  }

  def apply(name: String, chId: Int): StdStage = {
    new StdStage(name, chId)
  }
}

object RegStage {
  def apply(): RegStage = apply("RegStage")

  def apply(name: String): RegStage = {
    apply(name, 0)
  }

  def apply(name: String, chId: Int): RegStage = {
    new RegStage(name, chId)
  }
}

object MultiCycleStage {
  def apply(): SingleChannelStage = apply("MultiCycleStage")

  def apply(name: String, length: Int, pipe: Boolean, chId: Int): SingleChannelStage = {
    if (length == 0) {
      new StdStage(name, chId)
    } else {
      new MultiCycleStage(name, length, pipe, chId)
    }
  }

  def apply(name: String): SingleChannelStage = {
    apply(name, 1, true, 0)
  }

  def apply(name: String, length: Int): SingleChannelStage = {
    apply(name, length, true, 0)
  }

  def apply(name: String, length: Int, pipe: Boolean): SingleChannelStage = {
    apply(name, length, pipe, 0)
  }

  def apply(name: String, length: Int, chId: Int): SingleChannelStage = {
    apply(name, length, true, chId)
  }
}
