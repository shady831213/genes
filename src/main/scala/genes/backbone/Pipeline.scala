package genes.backbone

import scala.collection.mutable.ArrayBuffer
import chisel3._
import chisel3.experimental.{ChiselAnnotation, annotate}
import chisel3.experimental.RunFirrtlTransform
import firrtl._
import firrtl.annotations._
import firrtl.ir._

import scala.collection.mutable


trait Pipeline extends PipeDAG {
  type T <: Pipeline
  protected var plugins = ArrayBuffer[Plugin]()
  protected implicit val pipeline: Pipeline = this

  def Stages = stages

  def Stages(name: String) = stages(name)

  def AddPlugin(plugins: Plugin*): Unit = {
    for (plugin <- plugins) {
      this.plugins = this.plugins :+ plugin
    }
  }

  def Plugin[T](clazz: Class[T]): T = {
    val filtered = plugins.filter(o => clazz.isAssignableFrom(o.getClass))
    assert(filtered.length == 1, s"??? ${
      clazz.getName
    }")
    filtered.head.asInstanceOf[T]
  }

  protected [backbone] def PluginsBuildPass(): Unit = {
    plugins.foreach(_.setup())
    plugins.foreach(_.build())
  }

  protected [backbone] def DAGBuildPass(): Unit = {
    CheckStageablePass(this, Set[Stageable[Data]](), mutable.HashMap[Stageable[Data], Seq[PipeStage]]()) ::
      InsertStageablePass(this, Set[Stageable[Data]]()) ::
      ConnectPass(this) ::
      Nil foreach (_ ())
  }

  def build(): Unit = {
    PluginsBuildPass()
    DAGBuildPass()
    annotate(PipeLineDefChiselAnnotation(this))
  }

}

case class PipeLineDefAnnotation(top: Pipeline) extends NoTargetAnnotation

case class PipeLineDefChiselAnnotation(top: Pipeline) extends ChiselAnnotation with RunFirrtlTransform {
  override def toFirrtl: Annotation = PipeLineDefAnnotation(top)

  def transformClass: Class[PipeLineDefTransform] = classOf[PipeLineDefTransform]
}


class PipeLineDefTransform extends Transform {

  import firrtl.Mappers._

  def inputForm: CircuitForm = HighForm

  def outputForm: CircuitForm = HighForm


  def run(circuit: Circuit, annotations: AnnotationSeq): Circuit = {
    val ann = annotations
      .collect { case m: PipeLineDefAnnotation => m }.head

    val allOutputStageableString = mutable.Set[String]()

    def collectOutputString(dag: PipeDAG): Unit = {
      for (stage <- dag.stages) {
        stage match {
          case g: PipeDAG => collectOutputString(g)
          case n => {
            require(n.isInstanceOf[SingleChannelStage])
            n.asInstanceOf[SingleChannelStage].deqs.foreach(_.datas.values.foreach(allOutputStageableString += _.instanceName))
          }
        }
      }
    }

    def processModule(myModule: DefModule): DefModule = {
      def checkOutputeDeclareInConditional(c: Conditionally): Statement = {
        def checkOutputDeclare(statement: Statement): Statement = {
          statement map processStatements match {
            case w: DefWire => {
              require(!allOutputStageableString.contains(w.name), s"output ${w.name} in conditional block need default value!@${c.info}")
              w
            }
            case c: Conditionally => checkOutputeDeclareInConditional(c)
            case s => s
          }
        }

        c.conseq map checkOutputDeclare
        c.alt map checkOutputDeclare
        c
      }

      def processStatements(statement: Statement): Statement = {
        statement map processStatements match {
          case c: Conditionally => checkOutputeDeclareInConditional(c)
          case s => s
        }
      }

      if (myModule.name == ann.top.pipeName) {
        collectOutputString(ann.top)
        myModule map processStatements
      } else {
        myModule
      }
    }


    circuit.copy(modules = circuit.modules map processModule)
  }

  def execute(state: CircuitState): CircuitState = {
    state.copy(circuit = run(state.circuit, state.annotations))
  }
}
