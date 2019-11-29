package genes.example.design.GenesExample.pipeline.scalarPipe.csr

import genes.organs.rocket.config._
import genes.example.design.GenesExample.config._

class CSRParameters(val nonExistCSRException: Boolean)

case object CSRDefaultParameters extends CSRParameters(nonExistCSRException = false)

case object CSRConfig extends Field[CSRParameters](CSRDefaultParameters)

trait CSRParams extends GEParams {
  implicit val csrp: CSRParameters = p(CSRConfig)
}