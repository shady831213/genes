package genes.example.design.GenesExample.pipeline.scalarPipe.exception

import chisel3._
import chisel3.util._
import genes.example.design.GenesExample.pipeline._
import genes.organs.rocket.config.Parameters
import genes.example.design.GenesExample.modules.csr.{Mstatus, Xcause, Xtvec}
import genes.example.design.GenesExample.pipeline.scalarPipe._
import genes.example.design.GenesExample.spec.{CSRs, Causes}
import genes.bones._
import genes.example.design.GenesExample.spec.CSRConstants._


class Exception(implicit p: Parameters, pipeline: Pipeline) extends ScalarPipePlugin {
  val hadException = Wire(Bool())
  val nextPCClient = Services().NextPCService(ScalarPipe.Stages.last)
  val hasEPending = Wire(Bool()).suggestName("hasEPending")

  Services().DecodeFlushService(hasEPending || hadException || XRETValid(ScalarPipe.Stages.last))

  def XRETValid(s: PipeStage): Bool = s.enq.isFire && s.input(Stageables().CTRL).csr === CSR_I && isXRET(s.input(Stageables().IR))

  def isXRET(inst: Instruction): Bool = inst.ir(21, 20) === 2.U && inst.ir(19, 7) === 0.U


  def build(): Unit = {
    val exceptionCause = Reg(new ExceptionCause()).suggestName("exceptionCause")
    val epc = Reg(UInt(xlen.W)).suggestName("epc")
    hadException := RegNext(ScalarPipe.Stages.last.output(Stageables().XCPT).valid).suggestName("hadException")
    exceptionCause := ScalarPipe.Stages.last.output(Stageables().XCPT).payload
    epc := ScalarPipe.Stages.last.output(Stageables().PC)
    hasEPending := ScalarPipe.Stages.map(_.input(Stageables().XCPT).valid).foldLeft(false.B)(_ || _)

    ScalarPipe.Stages.head plug {
      //frontEnd exception
      asStart(Stageables().XCPT).valid := (input(Stageables().FETCHERR) || !input(Stageables().CTRL).legal) && enq.valid
      asStart(Stageables().XCPT).payload.code := MuxCase(0.U,
        Seq(input(Stageables().FETCHERR) -> Causes.fetch_access.U,
          !input(Stageables().CTRL).legal -> Causes.illegal_instruction.U
        ))
      asStart(Stageables().XCPT).payload.badAddr := Mux(!input(Stageables().CTRL).legal,input(Stageables().IR).ir, 0.U)
    }

    Services().ExceptionService.Collect.foreach { case (stage, exception) => {
      stage plug {
        output(Stageables().XCPT) := input(Stageables().XCPT)
        when(exception.valid) {
          output(Stageables().XCPT).valid := true.B
          output(Stageables().XCPT).payload := exception.payload
          enq.flush := true.B
        }
        when(deq.flush || hadException) {
          output(Stageables().XCPT).valid := false.B
        }
      }
    }
    }

    when(hadException) {
      //not support privilege
      ScalarPipe.Stages.last.enq.flush := true.B
      val mstatus: Mstatus = Services().CSRService.Listen(pipeline)(CSRs.mstatus)
      val mcause: Xcause = Services().CSRService.Listen(pipeline)(CSRs.mcause)
      val mepc: UInt = Services().CSRService.Listen(pipeline)(CSRs.mepc)
      val mtval: UInt = Services().CSRService.Listen(pipeline)(CSRs.mtval)
      mstatus.mie := false.B
      mstatus.mpie := mstatus.mie
      mstatus.mpp := 3.U
      mcause.interrupt := false.B
      mcause.exceptionCode := exceptionCause.code
      mepc := epc
      mtval := exceptionCause.badAddr
    }

    nextPCClient.valid := hadException
    nextPCClient.payload.nextPC := {
      val xtvec: Xtvec = Services().CSRService.Listen(pipeline)(CSRs.mtvec)
      (xtvec.base << 2).asUInt()
    }

    //RET

    ScalarPipe.Stages.last plug {
      when(XRETValid(thisStage)) {
        enq.flush := true.B
        //not support privilege
        nextPCClient.valid := input(Stageables().IR).privilege === 3.U
        nextPCClient.payload.nextPC := {
          val mstatus: Mstatus = Services().CSRService.Listen(pipeline)(CSRs.mstatus)
          val mepc: UInt = Services().CSRService.Listen(pipeline)(CSRs.mepc)
          mstatus.mpp := 0.U
          mstatus.mie := mstatus.mpie
          mstatus.mpie := true.B
          mepc
        }
        //is(1.U) {
        //assert(false, "not support privilege!")
        //}
      }
    }

  }
}
