package genes.organs.tester.memory

import chisel3._
import chisel3.core.{Bool, Clock, Input, Reset}
import firrtl.ir.Type
import firrtl_interpreter._
import treadle.executable.Transition
import treadle.{ScalaBlackBox, ScalaBlackBoxFactory}

trait MemoryBlackBoxImp extends BlackBoxImplementation with ScalaBlackBox {
  val mem: MemoryInterface
  val align: MemAlign.Value

  protected def _inputChanged(name: String, value: BigInt): Unit = {}

  protected def getError(inputValues: Seq[BigInt], tpe: Type): BigInt

  protected def errorExecute(inputValues: Seq[Concrete], tpe: Type): Concrete

  protected def _clockChange(transition: Transition, clockName: String = ""): Unit = {}
}

trait MemoryReadBlackBoxImp extends MemoryBlackBoxImp {
  protected def getRdata(inputValues: Seq[BigInt], tpe: Type): BigInt

  protected def rdataExecute(inputValues: Seq[Concrete], tpe: Type): Concrete
}

trait MemAsyncRead extends MemoryReadBlackBoxImp {
  override def outputDependencies(outputName: String): Seq[(String)] = {
    outputName match {
      case "rdata" => Seq("raddr")
      case "err" => Seq("raddr")
      case _ => Seq.empty
    }
  }

  protected def getRdata(inputValues: Seq[BigInt], tpe: Type): BigInt = {
    val raddr :: _ = inputValues
    if ((raddr & mem.addrMask) >= mem.low && (raddr & mem.addrMask) <= mem.high) {
      mem.Read(raddr * align.id, align)
    } else {
      0
    }
  }

  override protected def getError(inputValues: Seq[BigInt], tpe: Type): BigInt = {
    val raddr :: _ = inputValues
    if ((raddr & mem.addrMask) < mem.low || (raddr & mem.addrMask) > mem.high) 1 else 0
  }

  def cycle(): Unit = {}

  protected def rdataExecute(inputValues: Seq[Concrete], tpe: Type): Concrete = {
    val raddr :: _ = inputValues
    if ((raddr.value & mem.addrMask) >= mem.low && (raddr.value & mem.addrMask) <= mem.high) {
      ConcreteUInt(mem.Read(raddr.value, align), align.id * 8, raddr.poisoned).asUInt
    } else {
      ConcreteUInt(0, align.id * 8, raddr.poisoned).asUInt
    }
  }

  override protected def errorExecute(inputValues: Seq[Concrete], tpe: Type): Concrete = {
    val raddr :: _ = inputValues
    val result = if ((raddr.value & mem.addrMask) < mem.low || (raddr.value & mem.addrMask) > mem.high) 1 else 0
    ConcreteUInt(result, 1).asUInt
  }
}

trait MemSyncRead extends MemoryReadBlackBoxImp {
  var re: Boolean = false
  var raddr: BigInt = 0
  var rdata: BigInt = 0
  var rerror: BigInt = 0

  override def outputDependencies(outputName: String): Seq[(String)] = {
    outputName match {
      case "rdata" => Seq("raddr")
      case "err" => Seq("raddr")
      case _ => Seq.empty
    }
  }

  override protected def _inputChanged(name: String, value: BigInt): Unit = {
    name match {
      case "re" => re = value != 0
      case "raddr" => raddr = value * align.id
      case _ =>
    }
  }

  override protected def _clockChange(transition: Transition, clockName: String = ""): Unit = {
    if (re) {
      rerror = if ((raddr & mem.addrMask) < mem.low || (raddr & mem.addrMask) > mem.high) 1 else 0
      if (rerror == 0) {
        rdata = mem.Read(raddr, align)
      }
    }
  }

  protected def getRdata(inputValues: Seq[BigInt], tpe: Type): BigInt = {
    rdata
  }

  override protected def getError(inputValues: Seq[BigInt], tpe: Type): BigInt = {
    rerror
  }

  def cycle(): Unit = {}

  protected def rdataExecute(inputValues: Seq[Concrete], tpe: Type): Concrete = {
    ConcreteUInt(rdata, align.id * 8).asUInt
  }

  override protected def errorExecute(inputValues: Seq[Concrete], tpe: Type): Concrete = {
    ConcreteUInt(rerror, 1).asUInt
  }
}

trait MemSyncWrite extends MemoryBlackBoxImp {
  var we: Boolean = false
  var wmask: BigInt = 1
  var waddr: BigInt = 0
  var wdata: BigInt = 0
  var werror: BigInt = 0

  override def outputDependencies(outputName: String): Seq[(String)] = {
    outputName match {
      case "err" => Seq("waddr")
      case _ => Seq.empty
    }
  }

  override protected def _inputChanged(name: String, value: BigInt): Unit = {
    name match {
      case "we" => we = value != 0
      case "wmask" if align != MemAlign.Byte => wmask = value
      case "waddr" => waddr = value * align.id
      case "wdata" => wdata = value
      case _ =>
    }
    //    println(s"inputChange triggered, we = $we, waddr = $waddr, wdata = $wdata!")
  }

  override protected def _clockChange(transition: Transition, clockName: String = ""): Unit = {
    if (we) {
      werror = if ((waddr & mem.addrMask) < mem.low || (waddr & mem.addrMask) > mem.high) 1 else 0
      if (werror == 0) {
        mem.Write(waddr, wdata, align, wmask)
      }
    }
  }

  override protected def getError(inputValues: Seq[BigInt], tpe: Type): BigInt = {
    werror
  }

  override protected def errorExecute(inputValues: Seq[Concrete], tpe: Type): Concrete = {
    ConcreteUInt(werror, 1).asUInt
  }
}

class MemSyncWriteAsyncRead(val name: String, val mem: MemoryInterface, val align: MemAlign.Value) extends MemAsyncRead with MemSyncWrite {
  override def outputDependencies(outputName: String): Seq[(String)] = {
    super[MemAsyncRead].outputDependencies(outputName) ++ super[MemSyncWrite].outputDependencies(outputName)
  }

  override def clockChange(transition: Transition, clockName: String): Unit = {
    super[MemSyncWrite]._clockChange(transition, clockName)
    super[MemAsyncRead]._clockChange(transition, clockName)
  }

  override def inputChanged(name: String, value: BigInt): Unit = {
    super[MemSyncWrite]._inputChanged(name, value)
    super[MemAsyncRead]._inputChanged(name, value)
  }

  override def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    outputName match {
      case "rdata" => super[MemAsyncRead].getRdata(inputValues, tpe)
      case "err" => {
        super[MemAsyncRead].getError(inputValues, tpe) | super[MemSyncWrite].getError(inputValues, tpe)
      }
    }
  }

  override def execute(inputValues: Seq[Concrete], tpe: Type, outputName: String): Concrete = {
    outputName match {
      case "rdata" => super[MemAsyncRead].rdataExecute(inputValues, tpe)
      case "err" => {
        super[MemAsyncRead].errorExecute(inputValues, tpe) | super[MemSyncWrite].errorExecute(inputValues, tpe)
      }
    }
  }
}

class MemSyncWriteSyncRead(val name: String, val mem: MemoryInterface, val align: MemAlign.Value) extends MemSyncRead with MemSyncWrite {
  override def outputDependencies(outputName: String): Seq[(String)] = {
    super[MemSyncRead].outputDependencies(outputName) ++ super[MemSyncWrite].outputDependencies(outputName)
  }

  override def clockChange(transition: Transition, clockName: String): Unit = {
    super[MemSyncWrite]._clockChange(transition, clockName)
    super[MemSyncRead]._clockChange(transition, clockName)
  }

  override def inputChanged(name: String, value: BigInt): Unit = {
    super[MemSyncWrite]._inputChanged(name, value)
    super[MemSyncRead]._inputChanged(name, value)
  }

  override def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    outputName match {
      case "rdata" => super[MemSyncRead].getRdata(inputValues, tpe)
      case "err" => {
        super[MemSyncRead].getError(inputValues, tpe) | super[MemSyncWrite].getError(inputValues, tpe)
      }
    }
  }

  override def execute(inputValues: Seq[Concrete], tpe: Type, outputName: String): Concrete = {
    outputName match {
      case "rdata" => super[MemSyncRead].rdataExecute(inputValues, tpe)
      case "err" => {
        super[MemSyncRead].errorExecute(inputValues, tpe) | super[MemSyncWrite].errorExecute(inputValues, tpe)
      }
    }
  }
}

trait MemBBSyncIO extends Bundle {
  val clock: Clock = Input(Clock())
  val reset: Reset = Input(Reset())
}

trait MemBBIOBase extends Bundle {
  val align: MemAlign.Value
  val addrWidth: Int
  val err = Output(Bool())
}

trait MemBBWriteIO extends MemBBIOBase {
  val we = Input(Bool())
  val waddr = Input(UInt(addrWidth.W))
  val wmask = Input(UInt(align.id.W))
  val wdata = Input(UInt((align.id * 8).W))
}

trait MemBBAsyncReadIO extends MemBBIOBase {
  val raddr = Input(UInt(addrWidth.W))
  val rdata = Output(UInt((align.id * 8).W))
}

trait MemBBSyncReadIO extends MemBBAsyncReadIO {
  val re = Input(Bool())
}

class MemSyncWriteAsyncReadBBIO(val align: MemAlign.Value, val addrWidth: Int) extends MemBBSyncIO with MemBBWriteIO with MemBBAsyncReadIO

class MemSyncWriteAsyncReadBB(align: MemAlign.Value, addrWidth: Int) extends BlackBox {
  val io = IO(new MemSyncWriteAsyncReadBBIO(align, addrWidth))
}

class DWORDMEMSWAR(addrWidth: Int = 64) extends MemSyncWriteAsyncReadBB(MemAlign.DWord, addrWidth)

class WORDMEMSWAR(addrWidth: Int = 64) extends MemSyncWriteAsyncReadBB(MemAlign.Word, addrWidth)

class HWMEMSWAR(addrWidth: Int = 64) extends MemSyncWriteAsyncReadBB(MemAlign.HW, addrWidth)

class BYTEMEMSWAR(addrWidth: Int = 64) extends MemSyncWriteAsyncReadBB(MemAlign.Byte, addrWidth)

class MemSyncWriteSyncReadBBIO(val align: MemAlign.Value, val addrWidth: Int) extends MemBBSyncIO with MemBBWriteIO with MemBBSyncReadIO

class MemSyncWriteSyncReadBB(align: MemAlign.Value, addrWidth: Int) extends BlackBox {
  val io = IO(new MemSyncWriteSyncReadBBIO(align, addrWidth))
}

class DWORDMEMSWSR(addrWidth: Int = 64) extends MemSyncWriteSyncReadBB(MemAlign.DWord, addrWidth)

class WORDMEMSWSR(addrWidth: Int = 64) extends MemSyncWriteSyncReadBB(MemAlign.Word, addrWidth)

class HWMEMSWSR(addrWidth: Int = 64) extends MemSyncWriteSyncReadBB(MemAlign.HW, addrWidth)

class BYTEMEMSWSR(addrWidth: Int = 64) extends MemSyncWriteSyncReadBB(MemAlign.Byte, addrWidth)

class MemBevFactory(mem: MemoryInterface) extends BlackBoxFactory {
  def createInstance(instanceName: String, blackBoxName: String): Option[BlackBoxImplementation] = {
    blackBoxName match {
      case "DWORDMEMSWAR" => Some(add(new MemSyncWriteAsyncRead(instanceName, mem, MemAlign.DWord)))
      case "WORDMEMSWAR" => Some(add(new MemSyncWriteAsyncRead(instanceName, mem, MemAlign.Word)))
      case "HWMEMSWAR" => Some(add(new MemSyncWriteAsyncRead(instanceName, mem, MemAlign.HW)))
      case "BYTEMEMSWAR" => Some(add(new MemSyncWriteAsyncRead(instanceName, mem, MemAlign.Byte)))
      case "DWORDMEMSWSR" => Some(add(new MemSyncWriteSyncRead(instanceName, mem, MemAlign.DWord)))
      case "WORDMEMSWSR" => Some(add(new MemSyncWriteSyncRead(instanceName, mem, MemAlign.Word)))
      case "HWMEMSWSR" => Some(add(new MemSyncWriteSyncRead(instanceName, mem, MemAlign.HW)))
      case "BYTEMEMSWSR" => Some(add(new MemSyncWriteSyncRead(instanceName, mem, MemAlign.Byte)))
      case _ => None
    }
  }
}

class TreadleMemBevFactory(mem: MemoryInterface) extends ScalaBlackBoxFactory {
  def createInstance(instanceName: String, blackBoxName: String): Option[ScalaBlackBox] = {
    blackBoxName match {
      case "DWORDMEMSWAR" => Some(add(new MemSyncWriteAsyncRead(instanceName, mem, MemAlign.DWord)))
      case "WORDMEMSWAR" => Some(add(new MemSyncWriteAsyncRead(instanceName, mem, MemAlign.Word)))
      case "HWMEMSWAR" => Some(add(new MemSyncWriteAsyncRead(instanceName, mem, MemAlign.HW)))
      case "BYTEMEMSWAR" => Some(add(new MemSyncWriteAsyncRead(instanceName, mem, MemAlign.Byte)))
      case "DWORDMEMSWSR" => Some(add(new MemSyncWriteSyncRead(instanceName, mem, MemAlign.DWord)))
      case "WORDMEMSWSR" => Some(add(new MemSyncWriteSyncRead(instanceName, mem, MemAlign.Word)))
      case "HWMEMSWSR" => Some(add(new MemSyncWriteSyncRead(instanceName, mem, MemAlign.HW)))
      case "BYTEMEMSWSR" => Some(add(new MemSyncWriteSyncRead(instanceName, mem, MemAlign.Byte)))
      case _ => None
    }
  }
}
