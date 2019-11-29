package genes.example.design.GenesExample.modules.csr

import chisel3._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait CSRAccess {
  def canWrite: Boolean = false

  def canRead: Boolean = false
}

object CSRAccess {

  object WRITE_ONLY extends CSRAccess {
    override def canWrite: Boolean = true
  }

  object READ_ONLY extends CSRAccess {
    override def canRead: Boolean = true
  }

  object READ_WRITE extends CSRAccess {
    override def canWrite: Boolean = true

    override def canRead: Boolean = true
  }

  object NONE extends CSRAccess

}


trait CSRInterface {
  def onWrite(csrAddress: Int)(doThat: => Unit): Unit

  def onRead(csrAddress: Int)(doThat: => Unit): Unit

  def r(csrAddress: Int, bitOffset: Int, that: Data): Unit

  def w(csrAddress: Int, bitOffset: Int, that: Data): Unit

  def writeWhenValid(csrAddress: Int, bitOffset: Int, that: Data, valid: UInt => Bool): Unit

  def r2w(csrAddress: Int, bitOffset: Int, that: Data): Unit

//  def rw(csrAddress: Int, bitOffset: Int, that: Data): Unit = {
//    r(csrAddress, bitOffset, that)
//    w(csrAddress, bitOffset, that)
//  }
//
//  def rw(csrAddress: Int, thats: (Int, Data)*): Unit = for (that <- thats) rw(csrAddress, that._1, that._2)
//
//  def w(csrAddress: Int, thats: (Int, Data)*): Unit = for (that <- thats) w(csrAddress, that._1, that._2)
//
//  def writeWhenValid(csrAddress: Int, thats: (Int, Data, UInt => Bool)*): Unit = for (that <- thats) writeWhenValid(csrAddress, that._1, that._2, that._3)
//
//  def r(csrAddress: Int, thats: (Int, Data)*): Unit = for (that <- thats) r(csrAddress, that._1, that._2)
//
//  def rw[T <: Data](csrAddress: Int, that: T): Unit = rw(csrAddress, 0, that)
//
//  def w[T <: Data](csrAddress: Int, that: T): Unit = w(csrAddress, 0, that)
//
//  def writeWhenValid[T <: Data](csrAddress: Int, that: T, valid: UInt => Bool): Unit = writeWhenValid(csrAddress, 0, that, valid)
//
//  def r[T <: Data](csrAddress: Int, that: T): Unit = r(csrAddress, 0, that)

  def isWriting(csrAddress: Int): Bool = {
    val ret = false.B
    onWrite(csrAddress) {
      ret := true.B
    }
    ret
  }

  def isReading(csrAddress: Int): Bool = {
    val ret = false.B
    onRead(csrAddress) {
      ret := true.B
    }
    ret
  }
}

case class CSRWriteWhenValid(that: Data, bitOffset: Int, valid: UInt => Bool)

case class CSRWrite(that: Data, bitOffset: Int)

case class CSRRead(that: Data, bitOffset: Int)

case class CSRReadToWriteOverride(that: Data, bitOffset: Int) //Used for special cases, as MIP where there shadow stuff
case class CSROnWrite(doThat: () => Unit)

case class CSROnRead(doThat: () => Unit)

case class CSRMapping() extends CSRInterface {
  val accessMapping = mutable.HashMap[Int, ArrayBuffer[Any]]()
  val dataMapping = mutable.HashMap[Int, Data]()

  def addAccessMappingAt(address: Int, that: Any) = accessMapping.getOrElseUpdate(address, new ArrayBuffer[Any]) += that

  def addDataMapping[T <: Data](address: Int, that: T) = {
    require(dataMapping.get(address).isEmpty || dataMapping.get(address).isDefined && that == dataMapping(address))
    dataMapping(address) = that
  }

  def apply[T <: Data](address: Int): T = {
    dataMapping(address).asInstanceOf[T]
  }

  override def r(csrAddress: Int, bitOffset: Int, that: Data): Unit = addAccessMappingAt(csrAddress, CSRRead(that, bitOffset))

  override def w(csrAddress: Int, bitOffset: Int, that: Data): Unit = addAccessMappingAt(csrAddress, CSRWrite(that, bitOffset))

  override def writeWhenValid(csrAddress: Int, bitOffset: Int, that: Data, valid: UInt => Bool): Unit = addAccessMappingAt(csrAddress, CSRWriteWhenValid(that, bitOffset, valid))

  override def r2w(csrAddress: Int, bitOffset: Int, that: Data): Unit = addAccessMappingAt(csrAddress, CSRReadToWriteOverride(that, bitOffset))

  override def onWrite(csrAddress: Int)(body: => Unit): Unit = addAccessMappingAt(csrAddress, CSROnWrite(() => body))

  override def onRead(csrAddress: Int)(body: => Unit): Unit = addAccessMappingAt(csrAddress, CSROnRead(() => {
    body
  }))
}
