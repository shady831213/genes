package genes.backbone

import chisel3._
import chisel3.util._

import scala.collection.mutable.Stack

abstract class Plugin(implicit val pipeline: Pipeline) extends PipelineComponent {
  private var stageStack = Stack[PipeStage]()

  final protected def thisStage: PipeStage = {
    require(stageStack.nonEmpty, "Plugin APIs must be called inside plug!")
    stageStack.head
  }

  //plugin apis
  def enq = thisStage.enq

  def enqs = thisStage.enqs

  def deq = thisStage.deq

  def deqs = thisStage.deqs

  def Stages = {
    require(thisStage.isInstanceOf[PipeDAG], s"stage ${thisStage.pipeName} is not sub class of PipeDAG, Stages is only support by PipeDAG!")
    thisStage.asInstanceOf[PipeDAG].stages
  }

  def Stages(name: String) = {
    require(thisStage.isInstanceOf[PipeDAG], s"stage ${thisStage.pipeName} is not sub class of PipeDAG, Stages is only support by PipeDAG!")
    thisStage.asInstanceOf[PipeDAG].stages(name)
  }

  def getEnq(stage: PipeStage) = thisStage.getEnq(stage)

  def getDeq(stage: PipeStage) = thisStage.getDeq(stage)

  def index = thisStage.index

  def outputStages = thisStage.outputStages

  def inputStages = thisStage.inputStages

  def isHead = thisStage.isHead

  def isTail = thisStage.isTail

  def prevStages = thisStage.prevStages

  def nextStages = thisStage.nextStages

  def otherStages = thisStage.otherStages


  def asStart[T <: Data](key: Stageable[T]): T = thisStage.asStart(key)

  def input[T <: Data](key: Stageable[T], ch: Int = 0): T = {
    thisStage match {
      case n: SingleChannelStage => n.input(key)
      case n => n.input(key, ch)
    }
  }

  def output[T <: Data](key: Stageable[T], ch: Int = 0): T = {
    thisStage match {
      case n: SingleChannelStage => n.output(key)
      case n => n.output(key, ch)
    }
  }

  implicit class implicitsStage[T <: PipeStage](stage: T) {
    def plug(logic: => Unit): Unit = {
      stageStack.push(stage)
      logic
      stageStack.pop()
    }
  }

}