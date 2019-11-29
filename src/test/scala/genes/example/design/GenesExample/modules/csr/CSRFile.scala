package genes.example.design.GenesExample.modules.csr

import chisel3._
import genes.example.design.GenesExample.config._
import genes.example.design.GenesExample.spec._
import genes.organs.rocket.config.Parameters

class CSRFile(implicit val p:Parameters) extends GEParams{
  import CSRAccess._

  val csrMapping = CSRMapping()

  //val exceptionClient = new ExceptionClient()


  //RW
  val mstatus = RegInit(Mstatus(), Wire(Mstatus()).default).suggestName("mstatus")
  val misa = RegInit(new Misa(), Wire(new Misa()).default()).suggestName("misa")
  val medeleg = Reg(UInt(xlen.W)).suggestName("medeleg")
  val mideleg = Reg(UInt(xlen.W)).suggestName("mideleg")
  val mie = Reg(new MiX).suggestName("mie")
  val mtvec = Reg(new Xtvec).suggestName("mtvec")
  val mcounteren = Reg(new Xcounteren).suggestName("mcounteren")
  val mscratch = Reg(UInt(xlen.W)).suggestName("mscratch")
  val mepc = Reg(UInt(xlen.W)).suggestName("mepc")
  val mcause = Reg(new Xcause).suggestName("mcause")
  val mtval = Reg(UInt(xlen.W)).suggestName("mtval")
  val mip = Reg(new MiX).suggestName("mip")
  val mcycle = Reg(new Xcounter).suggestName("mcycle")
  val minstret = Reg(new Xcounter).suggestName("minstret")
  val vstart = Reg(UInt(xlen.W)).suggestName("vstart")
  val fcsr = Reg(new Fcsr).suggestName("fcsr")

  //not support privilege
  //READ_WRITE(CSRs.mstatus, mstatus,
  //xlen - 1 -> "sd",
  //34 -> "sxl",
  //32 -> "uxl",
  //22 -> "tsr",
  //21 -> "tw",
  //20 -> "tvm",
  //19 -> "mxr",
  //18 -> "sum",
  //17 -> "mprv",
  //15 -> "xs",
  //13 -> "fs",
  //11 -> "mpp",
  //8 -> "spp",
  //7 -> "mpie",
  //5 -> "spie",
  //4 -> "upie",
  //3 -> "mie",
  //1 -> "sie",
  //0 -> "uie"
  //)
  READ_WRITE(CSRs.mstatus, mstatus,
    32 -> ("uxl", (wdata: UInt) => wdata =/= 0.U),
  )
  //  READ_WRITE(CSRs.mstatus, mstatus,
  //    32 -> "uxl",
  //  )
  READ_ONLY(CSRs.mstatus, mstatus, 11 -> "mpp")
  READ_WRITE(CSRs.misa, misa,
    xlen - 2 -> "mxl",
    26 -> "wlrl",
    0 -> "extensions"
  )
  READ_WRITE(CSRs.medeleg, medeleg)
  READ_WRITE(CSRs.mideleg, mideleg)
  READ_WRITE(CSRs.mie, mie,
    11 -> "meix",
    9 -> "seix",
    8 -> "ueix",
    7 -> "mtix",
    5 -> "stix",
    4 -> "utix",
    3 -> "msix",
    1 -> "ssix",
    0 -> "usix"
  )
  READ_WRITE(CSRs.mtvec, mtvec, 2 -> "base", 0 -> "mode")
  READ_WRITE(CSRs.mcounteren, mcounteren, 3 -> "hpm", 2 -> "ir", 1 -> "tm", 0 -> "cy")
  READ_WRITE(CSRs.mscratch, mscratch)
  READ_WRITE(CSRs.mepc, mepc)
  READ_WRITE(CSRs.mcause, mcause, xlen - 1 -> "interrupt", 0 -> "exceptionCode")
  READ_WRITE(CSRs.mtval, mtval)
  READ_WRITE(CSRs.mip, mip,
    11 -> "meix",
    9 -> "seix",
    8 -> "ueix",
    7 -> "mtix",
    5 -> "stix",
    4 -> "utix",
    3 -> "msix",
    1 -> "ssix",
    0 -> "usix"
  )
  READ_WRITE(CSRs.mcycle, mcycle, 0 -> "low", 32 -> "high")
  READ_WRITE(CSRs.mcycleh, mcycle, 0 -> "high")
  READ_WRITE(CSRs.minstret, minstret, 0 -> "low", 32 -> "high")
  READ_WRITE(CSRs.minstreth, minstret, 0 -> "high")
  READ_WRITE(CSRs.fcsr, fcsr,
    0 -> "fflags",
    5 -> "frm",
    8 -> "vxsat",
    8 -> "vxrm")
  READ_WRITE(CSRs.vxrm, fcsr.vxrm)
  READ_WRITE(CSRs.vxsat, fcsr.vxsat)
  READ_ONLY(CSRs.vstart,vstart)

  //RO
  val mvendorid = Wire(new Mvendorid).suggestName("mvendorid")
  mvendorid.offset := 0.U
  mvendorid.bank := 0.U
  val marchid = WireInit(0.U(xlen.W)).suggestName("marchid")
  val mimpid = WireInit(0.U(xlen.W)).suggestName("mimpid")
  val mhartid = WireInit(0.U(xlen.W)).suggestName("mhartid")
  val vtype = Reg(new Vtype).suggestName("vtype")
  val vl = Reg(UInt(xlen.W)).suggestName("vl")

  READ_ONLY(CSRs.mvendorid, mvendorid, 0 -> "offset", 7 -> "bank")
  READ_ONLY(CSRs.marchid, marchid)
  READ_ONLY(CSRs.mimpid, mimpid)
  READ_ONLY(CSRs.mhartid, mhartid)
  READ_ONLY(CSRs.cycle, mcycle, 0 -> "low", 32 -> "high")
  READ_ONLY(CSRs.cycleh, mcycle, 0 -> "high")
  READ_ONLY(CSRs.instret, minstret, 0 -> "low", 32 -> "high")
  READ_ONLY(CSRs.instreth, minstret, 0 -> "high")
  READ_ONLY(CSRs.vtype, vtype,
    xlen-1 -> "vill",
    5 -> "vediv",
    2 -> "vsew",
    0 -> "vlmul")
  READ_ONLY(CSRs.vl, vl)

  implicit class CsrAccessPimper(csrAccess: CSRAccess) {
    def apply[T <: Record](csrAddress: Int, that: T, thats: (Int, String)*): Unit = {
      csrMapping.addDataMapping(csrAddress, that)

      if (csrAccess == `WRITE_ONLY` || csrAccess == `READ_WRITE`) {
        for (field <- thats) {
          if (that.elements.get(field._2).isDefined && field._1 + that.elements(field._2).getWidth - 1 < xlen) {
            csrMapping.w(csrAddress, field._1, that.elements(field._2))
          }
        }
      }

      if (csrAccess == `READ_ONLY` || csrAccess == `READ_WRITE`) {
        for (field <- thats) {
          if (that.elements.get(field._2).isDefined && field._1 + that.elements(field._2).getWidth - 1 < xlen) {
            csrMapping.r(csrAddress, field._1, that.elements(field._2))
          }
        }
      }
    }

    def apply[T <: Record](csrAddress: Int, that: T, field: (Int, (String, UInt => Bool))): Unit = {
      csrMapping.addDataMapping(csrAddress, that)

      if (csrAccess == `WRITE_ONLY` || csrAccess == `READ_WRITE`) {
        if (that.elements.get(field._2._1).isDefined && field._1 + that.elements(field._2._1).getWidth - 1 < xlen) {
          csrMapping.writeWhenValid(csrAddress, field._1, that.elements(field._2._1), field._2._2)
        }
      }

      if (csrAccess == `READ_ONLY` || csrAccess == `READ_WRITE`) {
        if (that.elements.get(field._2._1).isDefined && field._1 + that.elements(field._2._1).getWidth - 1 < xlen) {
          csrMapping.r(csrAddress, field._1, that.elements(field._2._1))
        }
      }
    }

    def apply(csrAddress: Int, that: Data): Unit = {
      csrMapping.addDataMapping(csrAddress, that)
      if (csrAccess == `WRITE_ONLY` || csrAccess == `READ_WRITE`) csrMapping.w(csrAddress, 0, that)
      if (csrAccess == `READ_ONLY` || csrAccess == `READ_WRITE`) csrMapping.r(csrAddress, 0, that)
    }
  }
}
