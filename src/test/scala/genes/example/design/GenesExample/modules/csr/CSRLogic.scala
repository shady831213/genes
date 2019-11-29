package genes.example.design.GenesExample.modules.csr

import chisel3._
import chisel3.util._
import genes.example.design.GenesExample.config.{GEBundle, _}
import genes.example.design.GenesExample.spec.CSRConstants._
import genes.organs.rocket.config._

class CSRSigs(implicit p: Parameters) extends GEBundle()(p) {
  val address = UInt(SZ_CSR_ADDR.W)
  val writeData = UInt(xlen.W)
  val readData = UInt(xlen.W)
  val readToWriteData = UInt(xlen.W)
  val readAccess = Bool()
  val writeAccess = Bool()
  val valid = Bool()
  val halt = Bool()
  val illegalAccess = Bool()
}

class CSRLogic(csrMapping: CSRMapping)(implicit val p: Parameters) extends GEParams {
  val sigs = Wire(new CSRSigs).suggestName("csr_sigs")
  val writeEn = WireInit(sigs.valid && sigs.writeAccess && !sigs.halt)
  val readEn = WireInit(sigs.valid && sigs.readAccess && !sigs.halt)
  sigs.illegalAccess := true.B
  sigs.readData := 0.U(xlen.W)
  sigs.readToWriteData := sigs.readData
  for ((addr, jobs) <- csrMapping.accessMapping) {
    when(sigs.address === addr.U) {
      val withWrite = jobs.exists(j => j.isInstanceOf[CSRWrite] || j.isInstanceOf[CSROnWrite])
      val withRead = jobs.exists(j => j.isInstanceOf[CSRRead] || j.isInstanceOf[CSROnRead])
      if (withRead && withWrite) {
        sigs.illegalAccess := false.B
      } else {
        if (withWrite) {
          when(sigs.writeAccess) {
            sigs.illegalAccess := false.B
          }
        }
        if (withRead) {
          when(sigs.readAccess) {
            sigs.illegalAccess := false.B
          }
        }
      }

      when(writeEn) {
        for (element <- jobs) element match {
          case element: CSRWriteWhenValid => when(element.valid(sigs.writeData(element.bitOffset + element.that.getWidth - 1, element.bitOffset))) {
            element.that := sigs.writeData(element.bitOffset + element.that.getWidth - 1, element.bitOffset)
          }
          case element: CSRWrite => element.that := sigs.writeData(element.bitOffset + element.that.getWidth - 1, element.bitOffset)
          case element: CSROnWrite =>
            element.doThat()
          case _ =>
        }
      }

      val readJobs = jobs.collect { case element: CSRRead => element.asInstanceOf[CSRRead] }
      if (readJobs.nonEmpty) {
        sigs.readData := readJobs.foldLeft(0.U(xlen.W)) {
          case (a, b) => a | {
            if (b.bitOffset != 0) Cat(b.that.asUInt(), 0.U.pad(b.bitOffset)) else b.that.asUInt()
          }
        }
      }

      when(readEn) {
        for (element <- jobs) element match {
          case element: CSROnRead =>
            element.doThat()
          case _ =>
        }
      }

      val readToWirteJobs = jobs.collect { case element: CSRReadToWriteOverride => element.asInstanceOf[CSRReadToWriteOverride] }
      if (readToWirteJobs.nonEmpty) {
        sigs.readToWriteData := readToWirteJobs.foldLeft(0.U(xlen.W)) {
          case (a, b) => a | {
            if (b.bitOffset != 0) Cat(b.that.asUInt(), 0.U.pad(b.bitOffset)) else b.that.asUInt()
          }
        }
      }
    }
  }

  when(!sigs.valid) {
    sigs.illegalAccess := false.B
  }
}

