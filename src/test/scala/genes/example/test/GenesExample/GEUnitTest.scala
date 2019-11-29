package genes.example.test.GenesExample

import genes.example.design.GenesExample.config._
import genes.organs.rocket.config.{Config, Parameters}
import org.scalatest.ParallelTestExecution

class WithRV32TestConfig extends Config((site, here, up) => {
  // Core
  case XLEN => 32
  case FETCH_WIDTH => 2
  case CORE_WIDTH => 2
}
)

class WithRV64TestConfig extends Config((site, here, up) => {
  // Core
  case XLEN => 64
  case FETCH_WIDTH => 2
  case CORE_WIDTH => 2
}
)

object RV32TestConfig extends GETestConfig {
  implicit val p: Parameters = new Config((site, here, up) => {
    // Core
    case XLEN => 32
    case FETCH_WIDTH => 2
    case CORE_WIDTH => 2
  }
  )
}

object RV64TestConfig extends GETestConfig {
  implicit val p: Parameters = new Config((site, here, up) => {
    // Core
    case XLEN => 64
    case FETCH_WIDTH => 2
    case CORE_WIDTH => 2
  }
  )
}

class GE32Regression extends GEFlatSpec(RV32TestConfig) with GE32Tests with ParallelTestExecution

class GE64Regression extends GEFlatSpec(RV64TestConfig) with GE64Tests with ParallelTestExecution


class GE32UnitTest extends GE32Regression with WithDebug

class GE64UnitTest extends GE64Regression with WithDebug
