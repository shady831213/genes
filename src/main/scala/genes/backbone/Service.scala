package genes.backbone

import genes.organs.rocket.config.Parameters
import chisel3._

import scala.collection.mutable
import scala.collection.concurrent.TrieMap

abstract class Service

abstract class ListenerServiceWithTransForm[S, L](toListener: S => L) extends Service with ListenImp[L] {
  protected val server = TrieMap[Pipeline, S]()

  def apply(server: S)(implicit pipeline: Pipeline): S = {
    require(this.server.get(pipeline).isEmpty)
    this.server(pipeline) = server
    server
  }

  def Listen(implicit pipeline: Pipeline) = listenCache.getOrElseUpdate(pipeline, toListener(server(pipeline)))
}

abstract class ListenerService[S] extends ListenerServiceWithTransForm[S, S](s => s)

trait ListenImp[L] {
  protected val listenCache = TrieMap[Pipeline, L]()

  def Listen(implicit pipeline: Pipeline): L
}


case class ClientInfo(stage: PipeStage, priority: Int, config: Parameters)

object NullClientInfo extends ClientInfo(null, 0, Parameters.empty)

class Client[T](data: T, val info: ClientInfo) {
  def apply(): T = data
}

object Client {
  def apply[T](data: T, info: ClientInfo): Client[T] = new Client(data, info)

  def apply[T](data: T): Client[T] = apply(data, NullClientInfo)

  def apply[T](data: T, stage: PipeStage, priority: Int, config: Parameters): Client[T] = apply(data, ClientInfo(stage, priority, config))

  def apply[T](data: T, stage: PipeStage, priority: Int): Client[T] = apply(data, ClientInfo(stage, priority, Parameters.empty))

  def apply[T](data: T, stage: PipeStage, config: Parameters): Client[T] = apply(data, ClientInfo(stage, 0, config))

  def apply[T](data: T, stage: PipeStage): Client[T] = apply(data, ClientInfo(stage, 0, Parameters.empty))

  def apply[T](data: T, config: Parameters): Client[T] = apply(data, ClientInfo(null, 0, config))

}


trait ClientsImp[T, V] {
  protected val clients = TrieMap[Pipeline, mutable.ArrayBuffer[Client[T]]]()
  protected val collectCache = TrieMap[Pipeline, V]()

  def apply(info: ClientInfo)(implicit pipeline: Pipeline): T

  def apply()(implicit pipeline: Pipeline): T = apply(NullClientInfo)

  def apply(stage: PipeStage, priority: Int)(implicit pipeline: Pipeline): T = apply(ClientInfo(stage, priority, Parameters.empty))

  def apply(stage: PipeStage, priority: Int, config: Parameters)(implicit pipeline: Pipeline): T = apply(ClientInfo(stage, priority, config))

  def apply(stage: PipeStage)(implicit pipeline: Pipeline): T = apply(ClientInfo(stage, 0, Parameters.empty))

  def apply(stage: PipeStage, config: Parameters)(implicit pipeline: Pipeline): T = apply(ClientInfo(stage, 0, config))

  def apply(config: Parameters)(implicit pipeline: Pipeline): T = apply(ClientInfo(null, 0, config))

  def apply(data: T)(implicit pipeline: Pipeline): T = apply(Client(data))

  def apply(client: Client[T])(implicit pipeline: Pipeline): T = {
    clients.getOrElseUpdate(pipeline, mutable.ArrayBuffer[Client[T]]()) += client
    client()
  }

  def Collect(implicit pipeline: Pipeline): V
}


abstract class CollectorServiceWithTransForm[T, V](toCollector: Seq[Client[T]] => V)(gen: => T = null) extends Service with ClientsImp[T, V] {
  def apply(info: ClientInfo)(implicit pipeline: Pipeline): T = {
    require(gen != null, s"gen is not defined by ${getClass.getName}, you can use apply(client: Client[T])(implicit pipeline: Pipeline)!")
    gen match {
      case d: Data => try {
        apply(Client(Wire(d).asInstanceOf[T], info))
      } catch {
        case ExpectedChiselTypeException(message) => apply(Client(d.asInstanceOf[T], info))
      }

      case o => apply(Client(o, info))
    }
  }

  def Collect(implicit pipeline: Pipeline) = collectCache.getOrElseUpdate(pipeline, toCollector(clients.getOrElse(pipeline, mutable.ArrayBuffer[Client[T]]())))

}

abstract class CollectorServiceReduce[T](reduceFn: (T, T) => T, default: Option[T] = None)(gen: => T = null) extends CollectorServiceWithTransForm[T, T](clients => {
  default match {
    case Some(d) => clients.map(_ ()).foldLeft(d)(reduceFn)
    case None => clients.map(_ ()).reduce(reduceFn)
  }
})(gen)

abstract class CollectorServiceToSeq[T](gen: => T = null) extends CollectorServiceWithTransForm[T, Seq[T]](clients => clients.map(_ ()))(gen)

abstract class CollectorServiceUnique[T](gen: => T = null) extends CollectorServiceWithTransForm[T, T](clients => {
  require(clients.length == 1)
  clients.map(_ ()).head
})(gen)


abstract class CollectorService[T](gen: => T = null) extends CollectorServiceWithTransForm[T, Seq[Client[T]]](v => v)(gen)


abstract class MixedService[T, V, L](toReporter: Seq[Client[T]] => V, toListener: Seq[Client[T]] => L)(gen: => T = null) extends CollectorServiceWithTransForm(toReporter)(gen) with ListenImp[L] {
  def Listen(implicit pipeline: Pipeline) = listenCache.getOrElseUpdate(pipeline, toListener(clients.getOrElse(pipeline, mutable.ArrayBuffer[Client[T]]())))
}


class ClientIf[T <: Data](gen: T) extends Bundle {
  val valid = Bool()
  val payload: T = gen.cloneType

  def default: ClientIf[T] = {
    valid := false.B
    this
  }

  override def cloneType: ClientIf.this.type = new ClientIf(gen).asInstanceOf[ClientIf.this.type]
}

object ClientIf {
  def apply[T <: Data](gen: T): ClientIf[T] = new ClientIf(gen)
}

class ClientDecoupleIf[T <: Data](gen: T) extends Bundle {
  val valid = Bool()
  val payload: T = gen.cloneType
  val ready = Bool()

  def default: ClientDecoupleIf[T] = {
    valid := false.B
    ready := true.B
    this
  }

  override def cloneType: ClientDecoupleIf.this.type = new ClientDecoupleIf(gen).asInstanceOf[ClientDecoupleIf.this.type]
}

object ClientDecoupleIf {
  def apply[T <: Data](gen: T): ClientDecoupleIf[T] = new ClientDecoupleIf(gen)
}

