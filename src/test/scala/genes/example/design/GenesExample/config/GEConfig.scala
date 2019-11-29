package genes.example.design.GenesExample.config

import chisel3._
import chisel3.core.{MultiIOModule, RawModule}
import genes.backbone._
import genes.organs.utils.MonitorIO
import genes.organs.rocket.config._

case object XLEN extends Field[Int]

case object CORE_WIDTH extends Field[Int](1)

case object FETCH_WIDTH extends Field[Int](1)

case object FETCH_ENTRIES extends Field[Int](0)

case object BUS_MXLEN extends Field[Int](16)

trait GEParams {
  implicit val p: Parameters
  implicit val xlen: Int = p(XLEN)
  val coreWidth: Int = p(CORE_WIDTH)
  val fetchWidth: Int = p(FETCH_WIDTH)
  val fetchEntries: Int = math.max(p(FETCH_ENTRIES), fetchWidth << 1)
  val busMxLen: Int = p(BUS_MXLEN)

  def xlen2XL: Int = xlen match {
    case 32 => 1
    case 64 => 2
    case 128 => 3
    case _ =>
      assert(false)
      0
  }
}

class GEBundle(implicit val p: Parameters) extends Bundle with GEParams

class GEMonitorIO[T <: Record](driverIO: T)(implicit val p: Parameters) extends MonitorIO(driverIO) with GEParams

abstract class GEPlugin(implicit val p: Parameters, pipeline: Pipeline) extends Plugin with GEParams

abstract class GEMultiIOModule(implicit val p: Parameters) extends MultiIOModule with GEParams

abstract class GERawModule(implicit val p: Parameters) extends RawModule with GEParams

