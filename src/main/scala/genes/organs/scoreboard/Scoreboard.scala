package genes.organs.scoreboard

import chisel3._

class Scoreboard(n: Int) {
  def set(ens: Vec[Bool], addrs: Vec[UInt]): Unit = update(ens.foldLeft(false.B)(_ || _), (ens zip addrs).map { case (en, addr) => mask(en, addr) }.foldLeft(_next.asUInt())(_ | _))

  def clear(ens: Vec[Bool], addrs: Vec[UInt]): Unit = update(ens.foldLeft(false.B)(_ || _), (ens zip addrs).map { case (en, addr) => (~mask(en, addr)).asUInt() }.foldLeft(_next.asUInt())(_ & _))

  def clear(en:Bool, addr: UInt): Unit = update(en, _next.asUInt() & (~mask(en, addr)).asUInt())

  def read(addrs: Vec[UInt]): Vec[Bool] = VecInit(addrs.map(read(_)))

  def read(addr: UInt): Bool = r(addr)

  def readAll: UInt = r

  def flush(): Unit = r := 0.U

  protected val r = RegInit(0.U(n.W)).suggestName("sb_bitmap")
  protected var _next = r
  protected var ens = false.B

  protected def mask(en: Bool, addr: UInt) = Mux(en, 1.U << addr, 0.U)

  def update(en: Bool, update: UInt): Unit = {
    _next = update
    ens = ens || en
    when(ens) {
      r := _next
    }
  }
}
