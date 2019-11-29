package genes.example.design.GenesExample.modules.csr

import chisel3._
import genes.example.design.GenesExample.config.{GEBundle, XLEN}
import genes.organs.rocket.config.Parameters

class Xtvec(implicit p: Parameters) extends GEBundle()(p) {
  val mode = UInt(2.W)
  val base = UInt((xlen - 2).W)
}

class Xcause(implicit p: Parameters) extends GEBundle()(p) {
  val interrupt = Bool()
  val exceptionCode = UInt((xlen - 1).W)
}

class Mstatus(implicit p: Parameters) extends GEBundle()(p) {
  val uie = Bool()
  val sie = Bool()
  val mie = Bool()
  val upie = Bool()
  val spie = Bool()
  val mpie = Bool()
  val spp = Bool()
  val mpp = UInt(2.W)
  val fs = UInt(2.W)
  val xs = UInt(2.W)
  val mprv = Bool()
  val sum = Bool()
  val mxr = Bool()
  val tvm = Bool()
  val tw = Bool()
  val tsr = Bool()
  val sd = Bool()

  def default: Mstatus = {
    uie := false.B
    sie := false.B
    mie := false.B
    upie := false.B
    spie := false.B
    mpie := false.B
    spp := false.B
    mpp := 3.U
    fs := 0.U
    xs := 0.U
    mprv := false.B
    sum := false.B
    mxr := false.B
    tvm := false.B
    tw := false.B
    tsr := false.B
    sd := false.B
    this
  }
}

class Mstatus64(implicit p: Parameters) extends Mstatus {
  val uxl = UInt(2.W)
  val sxl = UInt(2.W)

  override def default: Mstatus = {
    super.default
    uxl := xlen2XL.U
    sxl := 0.U
    this
  }
}


object Mstatus {
  def apply()(implicit p: Parameters): Mstatus = if (p(XLEN) == 32) new Mstatus else new Mstatus64
}


class Misa(implicit p: Parameters) extends GEBundle()(p) {
  val extensions = UInt(26.W)
  val wlrl = UInt((xlen - 28).W)
  val mxl = UInt(2.W)

  def getExtension(ext: Char*): UInt = {
    ext.map {
      _.toLower.toInt - 'a'.toInt
    }.foldLeft(0.U(26.W)) { case (a, b) => a | (1.U << b).asUInt }
  }

  def default(ext: Char*): Misa = {
    mxl := xlen2XL.U
    wlrl := 0.U
    extensions := getExtension(ext :+ 'i': _*)
    this
  }
}

class MiX extends Bundle {
  val usix = Bool()
  val ssix = Bool()
  val msix = Bool()
  val utix = Bool()
  val stix = Bool()
  val mtix = Bool()
  val ueix = Bool()
  val seix = Bool()
  val meix = Bool()
}

class Xcounteren extends Bundle {
  val cy = Bool()
  val tm = Bool()
  val ir = Bool()
  val hpm = UInt(29.W)
}

class Xcounter extends Bundle {
  val low = UInt(32.W)
  val high = UInt(32.W)
}

class Mvendorid extends Bundle {
  val offset = UInt(7.W)
  val bank = UInt(25.W)
}

class Vtype extends Bundle {
  val vill = Bool()
  val vediv = UInt(2.W)
  val vsew = UInt(3.W)
  val vlmul = UInt(2.W)
}

class Fcsr extends Bundle {
  val vxrm = UInt(2.W)
  val vxsat = Bool()
  val frm = UInt(3.W)
  val fflags = UInt(5.W)
}
