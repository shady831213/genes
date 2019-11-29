package genes.organs.tester.memory

trait WithMemory {
  def loadMem(file: String): Seq[BigInt]

  def memInit(data: Seq[BigInt], mem: MemoryInterface, start: BigInt = 0): Unit = {
    def addr(i: Int): BigInt = start + (i << 2)

    data.zipWithIndex.foreach { case (data, i) => mem.Write(addr(i), data, MemAlign.Word) }
  }
}

object MemAlign extends Enumeration {
  type MemAlign = Value
  val Byte = Value(1)
  val HW = Value(2)
  val Word = Value(4)
  val DWord = Value(8)
}


trait MemoryInterface {
  val low: BigInt
  val high: BigInt
  val addrMask: BigInt

  def WriteByte(Addr: BigInt, Data: BigInt)

  def ReadByte(Addr: BigInt): BigInt

  private def mask(align: MemAlign.Value): BigInt = align.id - 1

  def Write(Addr: BigInt, Data: BigInt, align: MemAlign.Value = MemAlign.Word, maskBits: BigInt = 0xffffffffL): Unit = {
    for (i <- 0 until align.id) {
      if (((maskBits >> i) & 0x1) == 0x1) {
        WriteByte(Addr & ~mask(align) | i & mask(align), (Data >> 8 * i) & BigInt(0xffL))
      }
    }
  }

  def Read(Addr: BigInt, align: MemAlign.Value = MemAlign.Word): BigInt = {
    Seq.fill(align.id)(Addr & ~mask(align)).zipWithIndex.map { case (hi, lo) => ReadByte(hi | lo & mask(align)) }.reduceRight { (a, b) => (b << 8) | (a & BigInt(0xffL)) }
  }
}


class BevMemory(val low: BigInt = 0, val high: BigInt = 0xffffffffL, val addrMask: BigInt = 0xffffffffL) extends MemoryInterface {
  private val mem = scala.collection.mutable.Map[BigInt, Byte]()

  def WriteByte(Addr: BigInt, Data: BigInt): Unit = {
    require((Data >> 8) == 0)
    val addr = Addr & addrMask
    require(addr >= low && addr <= high, f"Addr = ${addr}%x; low = $low%x; high = $high%x")
    mem(addr - low) = Data.toByte
    //println(f"write @ $Addr%x(${Addr - low}%x):${mem(Addr - low).toInt}%x")
  }

  def ReadByte(Addr: BigInt): BigInt = {
    val addr = Addr & addrMask
    require(addr >= low && addr <= high, f"Addr = ${addr}%x; low = $low%x; high = $high%x")
    //println(f"read @ $Addr%x(${Addr - low}%x):${mem(Addr - low).toInt}%x")
    try {
      mem(addr - low).toInt & 0xff
    } catch {
      case _: NoSuchElementException => {
        0
      }
    }
  }

}

