package genes.bones

import chisel3._

import scala.collection.immutable.ListMap

class Stageable[T <: Data](dataType: => T) extends PipelineNameable {
  def apply(): T = {
    dataType
  }
}


class StageableRecord(stageables: Stageable[Data]*) extends Record {
  val elements: ListMap[String, Data] = ListMap[String, Data](stageables.map { key => key.pipeName -> key() }: _*)

  override def cloneType: StageableRecord.this.type = new StageableRecord(stageables: _*).asInstanceOf[StageableRecord.this.type]
}

