package genes.backbone

import chisel3.Wire

protected[backbone] case class PipeEdge(start: PipeStage, target: PipeStage) extends PipelineNameable {
  override val pipeName = s"${start.pipeName}_to_${target.pipeName}"
  val enq = Wire(new PipeIO).suggestName(s"${pipeName}_enq")
  val deq = Wire(new PipeIO).suggestName(s"${pipeName}_deq")
  enq.default
  deq.default
  enq.pipeName = s"${pipeName}_input"
  deq.pipeName = s"${pipeName}_output"
  deq =~= enq

  def connect(): Unit = {
    deq =:= enq
  }
}
