package genes.bones

import chisel3._

import scala.collection.mutable

trait PipeDAG extends PipelineNameable {
  protected val stagesTable = mutable.HashMap[String, PipeStage]()

  def AddStage(stages: Seq[PipeStage]): Seq[PipeStage] = {
    for (stage <- stages) {
      AddStage(stage)
    }
    stages
  }

  def AddStage(stage: PipeStage): PipeStage = {
    require(this.stagesTable.get(stage.pipeName).isEmpty)
    stage.parent = this
    this.stagesTable(stage.pipeName) = stage
    stage
  }


  protected [bones] def stages: Seq[PipeStage] = stagesTable.values.toSeq.sortWith {StagesSortFn}

  protected [bones] def stages(name: String): PipeStage = {
    val stage = stagesTable(name)
    require(stage.isInstanceOf[PipeStage])
    stage.asInstanceOf[PipeStage]
  }

  def Stages: Seq[PipeStage]

  def Stages(name: String): PipeStage

  def HeadStages: Seq[PipeStage] = stages.filter(_.isHead)

  def TailStages: Seq[PipeStage] = stages.filter(_.isTail)

  def connection(c: => Unit):Unit = {
    c
    checkLoop()
    indexing()
  }

  implicit class StageConnection(start: PipeStage) {
    def -->(target: PipeStage): PipeStage = {
      require(stages.contains(start), s"use AddStage add ${start.pipeName} first!")
      require(stages.contains(target), s"use AddStage add ${target.pipeName} first!")

      val edge = PipeEdge(start, target)
      start.fanouts += edge
      target.fanins += edge
      target
    }

    def -->(targets: Seq[PipeStage]): Seq[PipeStage] = {
      targets.foreach {
        start --> _
      }
      targets
    }
  }

  implicit class StageConnections(starts: Seq[PipeStage]) {
    def -->(target: PipeStage): PipeStage = {
      starts.foreach(_ --> target)
      target
    }

    def -->(targets: Seq[PipeStage]): Seq[PipeStage] = {
      targets.foreach {
        starts --> _
      }
      targets
    }
  }

  private[bones] def checkLoop(): Unit = {
    val visited = mutable.Set[PipeStage]()
    for (stage <- stagesTable.values.toSeq.sortWith(_.fanins.length < _.fanins.length)) {
      val queue = mutable.ArrayBuffer[PipeStage]()
      if (!visited.contains(stage)) {
        dfs(stage, n => {
          require(!queue.contains(n), s"loop detected:${(queue.dropWhile(_ != n) :+ n).map(_.pipeName).mkString("-->")}!")
          queue += n
          false
        }
          , false, visited)
      }
    }
  }

  //reverse topsort order
  private[bones] def indexing(): Unit = {
    val visited = mutable.Set[PipeStage]()
    val queue = mutable.ArrayBuffer[PipeStage]()
    for (stage <- stagesTable.values.toSeq.sortWith(_.fanins.length < _.fanins.length)) {
      if (!visited.contains(stage)) {
        dfs(stage, n => {
          queue += n
          false
        }
          , false, visited, false)
      }
    }
    (queue.reverse zipWithIndex) foreach { case (n, i) => {
      n.id = i
    }
    }
  }
}

sealed abstract class PipeDAGPass(dag: PipeDAG) {
  def apply(): this.type
}

sealed abstract class DataFlowAnalysisPass[T](dag: PipeDAG, defaultSet: Set[T], forward: Boolean) extends PipeDAGPass(dag) {
  def postConstraint(i: Int): Unit

  def constraint(i: Int): Unit

  def transfer(i: Int): Unit

  def postTransfer(i: Int): Unit

  def apply(): this.type = {
    for (i <- {
      if (forward) dag.stages.indices else dag.stages.indices.reverse
    }) {
      constraint(i)
      postConstraint(i)
      transfer(i)
      postTransfer(i)
    }
    this
  }
}

private[bones] case class ConnectPass(dag: PipeDAG) extends PipeDAGPass(dag) {
  def apply(): this.type = {
    for (stage <- dag.stages) {
      stage.connectEnq()
      stage match {
        case g: PipeDAG => ConnectPass(g)()
        case _ =>
      }
      stage.connectDeq()
      stage.fanouts.foreach(edge => {
        edge.connect()
      })
    }
    this
  }
}

private[bones] case class CheckStageablePass(dag: PipeDAG, defaultFaninSet: Set[Stageable[Data]], StartTraceTable: mutable.HashMap[Stageable[Data], Seq[PipeStage]]) extends DataFlowAnalysisPass[Stageable[Data]](dag, defaultFaninSet, true) {
  val avalabilityFaninSetTable = mutable.ArrayBuffer.fill(dag.stages.length)(Set[Stageable[Data]]())
  val avalabilityFanoutSetTable = mutable.ArrayBuffer.fill(dag.stages.length)(Set[Stageable[Data]]())

  def tailAvalabilityFanoutSetTable = dag.TailStages.map(dag.stages.indexOf(_)).map(idx => avalabilityFanoutSetTable(idx)).reduce(_ ++ _)

  def postConstraint(i: Int): Unit = {
    val stage = dag.stages(i)
    for (key <- stage.starts.keys) {
      StartTraceTable(key) = StartTraceTable.getOrElse(key, Seq[PipeStage]()) :+ stage
      require(!avalabilityFaninSetTable(i).contains(key), s"asStart ${key.pipeName} redefined! startStages:${StartTraceTable(key).map(_.pipeName).mkString(",")}")
    }
  }

  def constraint(i: Int): Unit = {
    val stage = dag.stages(i)
    avalabilityFaninSetTable(i) = {
      if (stage.isHead)
        defaultFaninSet
      else
        stage.inputStages.map(_.id).map(idx => avalabilityFanoutSetTable(idx)).reduce(_ ++ _)
    }
  }

  def transfer(i: Int): Unit = {
    val stage = dag.stages(i)
    stage match {
      case g: PipeDAG => {
        val avalability = CheckStageablePass(g, avalabilityFaninSetTable(i) ++ stage.starts.keys.toSet, StartTraceTable)()
        avalabilityFanoutSetTable(i) = avalability.tailAvalabilityFanoutSetTable ++ stage.starts.keys.toSet
      }
      case _ => avalabilityFanoutSetTable(i) = avalabilityFaninSetTable(i) ++ stage.starts.keys.toSet
    }
  }

  def postTransfer(i: Int): Unit = {
    val stage = dag.stages(i)
//    println(s"stage:${stage.pipeName}:${avalabilityFanoutSetTable(i).map(_.pipeName).mkString(",")};${avalabilityFaninSetTable(i).map(_.pipeName).mkString(",")};")
    for (key <- stage.enqs.map(_.datas.keys.toSet).reduce(_ ++ _)) {
      require(avalabilityFanoutSetTable(i).contains(key), s"input ${key.pipeName} in stage ${stage.pipeName} is referred before declare asStart()!")
    }
    for (key <- stage.deqs.map(_.datas.keys.toSet).reduce(_ ++ _)) {
      require(avalabilityFanoutSetTable(i).contains(key), s"output ${key.pipeName} in stage ${stage.pipeName} is referred before declare asStart()!")
    }
  }
}


private[bones] case class InsertStageablePass(dag: PipeDAG, defaultFanoutSet: Set[Stageable[Data]]) extends DataFlowAnalysisPass[Stageable[Data]](dag, defaultFanoutSet, false) {
  val activeInputFaninSetTable = mutable.ArrayBuffer.fill(dag.stages.length)(Set[Stageable[Data]]())
  val activeOutputFaninSetTable = mutable.ArrayBuffer.fill(dag.stages.length)(Set[Stageable[Data]]())
  val activeFanoutSetTable = mutable.ArrayBuffer.fill(dag.stages.length)(Set[Stageable[Data]]())

  def headActiveInputFaninSetTable = dag.HeadStages.map(dag.stages.indexOf(_)).map(idx => {activeInputFaninSetTable(idx)}).reduce(_ ++ _)

  def headActiveOutputFaninSetTable = dag.HeadStages.map(dag.stages.indexOf(_)).map(idx => activeOutputFaninSetTable(idx)).reduce(_ ++ _) intersect dag.TailStages.map(dag.stages.indexOf(_)).map(idx => activeOutputFaninSetTable(idx)).reduce(_ ++ _)

  def starts = dag.stages.map(s => s.starts.keys.toSeq).reduce(_ ++ _)

  def postConstraint(i: Int): Unit = {
  }

  def constraint(i: Int): Unit = {
    val stage = dag.stages(i)
    activeFanoutSetTable(i) = {
      if (stage.isTail)
        defaultFanoutSet
      else
        stage.outputStages.map(_.id).map(idx => activeInputFaninSetTable(idx) ++ activeOutputFaninSetTable(idx)).reduce(_ ++ _)
    }
  }

  def transfer(i: Int): Unit = {
    val stage = dag.stages(i)
    stage match {
      case g: PipeDAG => {
        val activity = InsertStageablePass(g, activeFanoutSetTable(i) ++ stage.deqs.map(_.datas.keys.toSet).reduce(_ ++ _))()
        activeInputFaninSetTable(i) = activity.headActiveInputFaninSetTable ++ stage.enqs.map(_.datas.keys.toSet).reduce(_ ++ _) -- activity.starts -- stage.starts.keys.toSet
        activeOutputFaninSetTable(i) = activity.headActiveOutputFaninSetTable -- stage.starts.keys.toSet -- activity.starts
      }
      case _ => {
        activeInputFaninSetTable(i) = activeFanoutSetTable(i) ++ stage.enqs.map(_.datas.keys.toSet).reduce(_ ++ _) ++ stage.deqs.map(_.datas.keys.toSet).reduce(_ ++ _) -- stage.starts.keys.toSet
        activeOutputFaninSetTable(i) = activeFanoutSetTable(i) ++ stage.deqs.map(_.datas.keys.toSet).reduce(_ ++ _) -- stage.starts.keys.toSet
      }
    }
    //println(s"stage ${stage.pipeName}; i $i; ${activeFanoutSetTable(i).map(_.pipeName).mkString(",")};${activeInputFaninSetTable(i).map(_.pipeName).mkString(",")};${activeOutputFaninSetTable(i).map(_.pipeName).mkString(",")}")
  }

  def postTransfer(i: Int): Unit = {
    val stage = dag.stages(i)
    for (key <- activeInputFaninSetTable(i)) {
      //      stage.enqs.foreach(_.data(key))
      stage.enq.data(key)
      stage.fanins.foreach(_.deq.data(key))
    }
    for (key <- activeOutputFaninSetTable(i)) {
      stage.deqs.foreach(_.data(key))
    }
    for (key <- activeFanoutSetTable(i)) {
      //      stage.deqs.foreach(_.data(key))
      stage.deq.data(key)
      stage.fanouts.foreach(_.enq.data(key))
    }
    for ((key, stageable) <- stage.starts) {
      stage.enqs.foreach(_.data(key))
    }
  }
}