package genes

import chisel3._
import chisel3.util._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.concurrent.TrieMap

package object bones {

  private[bones] val dummyStageMagicName = "201001200x5f3759df"

  trait PipelineNameable {
    def pipeName: String = getClass.getName.split('.').last.replace("$", "")
  }

  trait PipelinePhase {
    def setup(): Unit = {}

    def build(): Unit
  }

  trait PipelineComponent extends PipelineNameable with PipelinePhase {
    protected implicit val pipeline: Pipeline
  }

  def StagesSortFn(a: PipeStage, b: PipeStage): Boolean = {
    if (a.chId == b.chId) {
      a.id < b.id
    } else {
      a.chId < b.chId
    }
  }

  protected[bones] def dfs(node: PipeStage, func: PipeStage => Boolean,
                           revert: Boolean = false,
                           visited: mutable.Set[PipeStage] = null,
                           preOrder: Boolean = true): Boolean = {
    def _visited = if (visited != null) visited else mutable.Set[PipeStage]()

    if (preOrder) {
      if (func(node)) {
        _visited += node
        return true
      }
    }
    for (edge <- {
      val edges = if (revert) node.fanins else node.fanouts
      if (preOrder) edges else edges.reverse
    }; target = {
      if (revert) edge.start else edge.target
    }) {
      if (!_visited.contains(target)) {
        if (dfs(target, func, revert, _visited, preOrder)) {
          return true
        }
      }
    }
    if (!preOrder) {
      if (func(node)) {
        _visited += node
        return true
      }
    }
    _visited += node
    false
  }

  protected[bones] def bfs(node: PipeStage, func: PipeStage => Boolean, revert: Boolean = false, visited: mutable.Set[PipeStage] = null): Unit = {
    val _visited = if (visited != null) visited else mutable.Set[PipeStage]()
    val q = ListBuffer[PipeStage]()
    if (visitNode(node)) return
    while (q.nonEmpty) {
      val n = q.head
      q -= n
      for (edges <- {
        if (revert) n.fanins else n.fanouts
      }; target = {
        if (revert) edges.start else edges.target
      }) {
        if (visitNode(target)) {
          return
        }
      }

    }

    def visitNode(node: PipeStage): Boolean = {
      if (!_visited.contains(node)) {
        q += node
        _visited += node
        return func(node)
      }
      false
    }
  }

  trait WithImplicitWrapper[T] {
    implicit val p: T
  }

  abstract class WithImplicitFactory[T, W <: WithImplicitWrapper[T]] {
    private val instances = TrieMap[T, W]()

    protected def gen(implicit p: T): W

    def apply()(implicit p: T): W = instances.getOrElseUpdate(p, {
      gen
    })
  }


  //service
  implicit class ClientsUtils[T](clients: Seq[Client[T]]) {
    def StageClientsMap: Map[PipeStage, Seq[Client[T]]] = {
      val map = mutable.HashMap[PipeStage, mutable.ArrayBuffer[Client[T]]]()
      clients.foreach(c => {
        map.getOrElseUpdate(c.info.stage, mutable.ArrayBuffer()) += c
      })
      map.toMap
    }

    def sortedByStage: Seq[Client[T]] = {
      clients.sortWith {
        case (a, b) => {
          StagesSortFn(a.info.stage, b.info.stage)
        }
      }
    }

    def sortedByPriority: Seq[Client[T]] = {
      clients.sortWith {
        case (a, b) => {
          a.info.priority > b.info.priority
        }
      }
    }
  }

  implicit class ClientIfReducer[T <: Data](seq: Seq[ClientIf[T]]) {
    def merge: ClientIf[T] = {
      require(seq.nonEmpty)
      val result = Wire(seq.head.cloneType)
      result.valid := seq.map {
        _.valid
      }.reduce(_ || _)
      result.payload := PriorityMux(seq.map {
        _.valid
      }, seq.map {
        _.payload
      })
      result
    }
  }

  implicit class ClientIfMapReducer[T <: Data](map: Map[PipeStage, Seq[Client[ClientIf[T]]]]) {
    def merge: Map[PipeStage, ClientIf[T]] = map.map { case (stage, clients) => {
      stage -> clients.sortedByPriority.map(_ ()).merge
    }
    }
  }

  implicit class StagesOptions(stages: Seq[PipeStage]) {
    def excludeDummy: Seq[PipeStage] = stages.filterNot(_.isInstanceOf[dummyStage])
  }

}