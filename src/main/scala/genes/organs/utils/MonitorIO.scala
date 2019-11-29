package genes.organs.utils

import chisel3._

abstract class MonitorIO[T <: Record](driverIO: T) extends Bundle {
  def bind(): Unit = {
    elements.toSeq.foreach { case (name, data) => data := driverIO.elements(name) }
  }
}
