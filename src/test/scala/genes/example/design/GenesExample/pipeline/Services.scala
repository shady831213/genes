package genes.example.design.GenesExample.pipeline

import chisel3._
import chisel3.util._
import genes.example.design.GenesExample.config.GEParams
import genes.example.design.GenesExample.modules.csr.{CSRFile, CSRMapping}
import genes.example.design.GenesExample.spec.Causes
import genes.backbone._
import genes.organs.rocket.config.Parameters

private[pipeline] class Services(implicit val p: Parameters) extends GEParams with WithImplicitWrapper[Parameters] {

  case object CSRService extends ListenerServiceWithTransForm[CSRFile, CSRMapping](s => s.csrMapping)

  case object ExceptionService extends CollectorServiceWithTransForm[ClientIf[ExceptionCause], Seq[(PipeStage, ClientIf[ExceptionCause])]](clients => {
    clients.StageClientsMap.merge.toSeq.sortWith(_._1.index < _._1.index)
  })(ClientIf(new ExceptionCause))

  case object NextPCService extends CollectorServiceWithTransForm[ClientIf[NextPC], ClientIf[NextPC]](clients => {
    clients.StageClientsMap.merge.toSeq.sortWith((a, b) => a._1.index < b._1.index).map(_._2).merge
  })(ClientIf(new NextPC())) {
    override def apply(client: Client[ClientIf[NextPC]])(implicit pipeline: Pipeline): ClientIf[NextPC] = {
      val exceptionClient = ExceptionService(client.info.stage, Causes.ExceptionPriority(Causes.misaligned_fetch))
      exceptionClient.valid := client().valid && client().payload.nextPC(1, 0) =/= 0.U
      exceptionClient.payload.default.code := Causes.misaligned_fetch.U
      super.apply(client)(pipeline)
    }
  }

  case object ScalarBusyService extends MixedService[Bool, Map[PipeStage, Bool], Bool](clients => {
    clients.StageClientsMap.map { case (n, c) => n -> c.map(_ ()).foldLeft(false.B)(_ || _) }
  },
    clients => {
      clients.StageClientsMap.map { case (n, c) => n -> c.map(_ ()).foldLeft(false.B)(_ || _) }.values.foldLeft(false.B)(_ || _)
    }
  )(Bool())

  case object DecodeFlushService extends CollectorServiceReduce[Bool]((a, b) => a || b,Some(false.B))(Bool())

  case object RegFileReadPortService extends CollectorServiceToSeq[RegisterFilePort](new RegisterFilePort(log2Ceil(32), xlen))

  case object RegFileBypassPortService extends CollectorServiceWithTransForm[ClientIf[RegisterFilePort], Seq[ClientIf[RegisterFilePort]]](
    clients => clients.StageClientsMap.merge.toSeq.sortWith(_._1.index < _._1.index).map(_._2)
  )(ClientIf(new RegisterFilePort(log2Ceil(32), xlen)))

  case object RegFileLongLatancyWritePortService extends CollectorService[ClientIf[RegisterFilePort]]

  case object RegFileWritePortService extends CollectorServiceToSeq[ClientIf[RegisterFilePort]](ClientIf(new RegisterFilePort(log2Ceil(32), xlen)))

  case object RegFileMonitorService extends CollectorService[(UInt, UInt)]


  case object WriteRegFileService extends CollectorServiceWithTransForm[ClientIf[UInt], Seq[(PipeStage, ClientIf[UInt])]](clients => {
    clients.StageClientsMap.merge.toSeq.sortWith((a, b) => a._1.index < b._1.index)
  })(ClientIf(UInt(xlen.W)))

}


object Services extends WithImplicitFactory[Parameters, Services] {
  protected def gen(implicit p: Parameters) = new Services
}