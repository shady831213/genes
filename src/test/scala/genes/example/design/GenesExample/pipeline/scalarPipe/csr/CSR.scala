package genes.example.design.GenesExample.pipeline.scalarPipe.csr

import chisel3._
import chisel3.util._
import genes.organs.rocket.config.Parameters
import genes.example.design.GenesExample.pipeline._
import genes.example.design.GenesExample.spec._
import genes.example.design.GenesExample.spec.CSRConstants._
import genes.example.design.GenesExample.spec.ScalarOpConstants._
import genes.example.design.GenesExample.modules.csr.{CSRFile, CSRLogic}
import genes.example.design.GenesExample.pipeline.scalarPipe._
import genes.organs.utils._
import genes.backbone._


class CSR(implicit p: Parameters, pipeline: Pipeline) extends ScalarPipePlugin with CSRParams {

  val csrFile = Services().CSRService(new CSRFile)
  val writeRegFileClient = Services().WriteRegFileService(ScalarPipe.Stages("memAccess"))

  val exceptionClient = Services().ExceptionService(ScalarPipe.Stages("execute"), Causes.ExceptionPriority(Causes.illegal_instruction))


  def build(): Unit = {
    ScalarPipe.Stages("execute") plug {
      val writeSrc = Mux(input(Stageables().IR).ir(14), input(Stageables().IR).rs1.pad(xlen), input(Stageables().RS)(0))
      val readToWriteData = Wire(UInt(xlen.W))
      val illegalAccess = Wire(Bool())

      val csrLogic = new CSRLogic(csrFile.csrMapping)
      csrLogic.sigs.address := input(Stageables().IR).csr
      csrLogic.sigs.readAccess := input(Stageables().CSR_READ)
      csrLogic.sigs.writeAccess := input(Stageables().CSR_WRITE)
      csrLogic.sigs.valid := enq.isFire && input(Stageables().CTRL).csr_valid
      csrLogic.sigs.halt := !deq.ready
      csrLogic.sigs.writeData := MuxLookup(input(Stageables().CTRL).csr, writeSrc, Seq(
        CSR_C.U -> (readToWriteData & (~writeSrc).asUInt()),
        CSR_S.U -> (readToWriteData | writeSrc)
      ))
      asStart(Stageables().CSR_OUT) := csrLogic.sigs.readData
      readToWriteData := csrLogic.sigs.readToWriteData
      illegalAccess := csrLogic.sigs.illegalAccess

      asStart(Stageables().CSR_WRITE) := !(input(Stageables().IR).ir(14, 13) === "b01".U && input(Stageables().IR).rs1 === 0.U ||
        input(Stageables().IR).ir(14, 13) === "b11".U && input(Stageables().IR).imm(IMM_Z.U) === 0.S)
      asStart(Stageables().CSR_READ) := input(Stageables().IR).ir(13, 7) =/= "b0100000".U

      if (csrp.nonExistCSRException) {
        exceptionClient.valid := illegalAccess
        exceptionClient.payload.code := Causes.illegal_instruction.U
        exceptionClient.payload.badAddr := input(Stageables().IR).ir
      } else {
        exceptionClient.default
        exceptionClient.payload.default
      }
    }

    ScalarPipe.Stages("memAccess") plug {
      writeRegFileClient.valid := input(Stageables().CSR_READ) && input(Stageables().CTRL).csr_valid
      writeRegFileClient.payload := input(Stageables().CSR_OUT)
    }

  }
}