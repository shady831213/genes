package genes.example.design.GenesExample.pipeline.scalarPipe.loadStore

import chisel3._
import chisel3.util.MuxLookup
import genes.organs.rocket.config.Parameters
import genes.example.design.GenesExample.spec.MemoryOpConstants._
import genes.example.design.GenesExample.pipeline._
import genes.example.design.GenesExample.modules.loadStore._
import genes.example.design.GenesExample.pipeline.scalarPipe._
import genes.example.design.GenesExample.spec.Causes
import genes.backbone._


class LoadStore(io: LoadStoreIO)(implicit p: Parameters, pipeline: Pipeline) extends ScalarPipePlugin {
  val loadMaExceptionClient = Services().ExceptionService(ScalarPipe.Stages("memAccess"), Causes.ExceptionPriority(Causes.misaligned_load))
  val loadAccessExceptionClient = Services().ExceptionService(ScalarPipe.Stages("memAccess").outputStages.head, Causes.ExceptionPriority(Causes.load_access))
  val storeMaExceptionClient = Services().ExceptionService(ScalarPipe.Stages("memAccess"), Causes.ExceptionPriority(Causes.misaligned_store))
  val storeAccessExceptionClient = Services().ExceptionService(ScalarPipe.Stages("memAccess").outputStages.head, Causes.ExceptionPriority(Causes.store_access))
  val loadStoreLogic = new {
    val cmdR = Reg(new LoadStoreCmdCh()).suggestName("cmdR")
    val start = RegInit(false.B).suggestName("startTrans")
    when(io.cmd.valid && !io.cmd.bits.wr) {
      cmdR := io.cmd.bits
    }
    when(io.resp.fire()) {
      start := false.B
    }
    when(io.cmd.fire()) {
      start := true.B
    }

    def waiting = start && !io.resp.fire()

    val cmd = Mux(io.cmd.valid && !io.cmd.bits.wr, io.cmd.bits, cmdR).suggestName("cmd")
    io.wdata.valid := false.B
    io.rdata.ready := true.B
    io.cmd.bits.len := 0.U
    io.wdata.bits.last := true.B

    def misAlign(cmd: LoadStoreCmdCh) = MuxLookup(cmd.size, 0.U, Seq(
      "b01".U -> cmd.addr(0),
      "b10".U -> cmd.addr(1, 0),
      "b11".U -> cmd.addr(2, 0),
    )) =/= 0.U

    def padRData(cmd: LoadStoreCmdCh, extFlag: Bool, data: UInt) = MuxLookup(cmd.size, data, Seq(
      "b00".U -> Mux(extFlag, (data >> bitOffset(byteOffset(cmd.addr))) (7, 0).asSInt().pad(xlen).asUInt(), (data >> bitOffset(byteOffset(cmd.addr))) (7, 0)),
      "b01".U -> Mux(extFlag, (data >> bitOffset(byteOffset(cmd.addr))) (15, 0).asSInt().pad(xlen).asUInt(), (data >> bitOffset(byteOffset(cmd.addr))) (15, 0)),
      "b10".U -> Mux(extFlag, (data >> bitOffset(byteOffset(cmd.addr))) (31, 0).asSInt().pad(xlen).asUInt(), (data >> bitOffset(byteOffset(cmd.addr))) (31, 0)),
    ))
  }
  val writeRegFileClient = Services().WriteRegFileService(ScalarPipe.Stages("memAccess").outputStages.head)

  def build(): Unit = {
    ScalarPipe.Stages("memAccess") plug {
      io.cmd.valid := input(Stageables().CTRL).mem && enq.isFire && !storeMaExceptionClient.valid && !loadMaExceptionClient.valid
      io.cmd.bits.wr := input(Stageables().CTRL).mem && isWrite(input(Stageables().CTRL).mem_cmd)
      io.cmd.bits.addr := input(Stageables().ALU_OUT)
      io.cmd.bits.size := input(Stageables().IR).sizeBits
      io.wdata.bits.data := input(Stageables().RS)(1) << bitOffset(byteOffset(io.cmd.bits.addr))
      io.wdata.bits.mask := getHeadMask(io.cmd.bits.addr, io.cmd.bits.size)
      val misAlign = WireInit(input(Stageables().CTRL).mem && enq.isFire && loadStoreLogic.misAlign(io.cmd.bits))
      loadMaExceptionClient.valid := misAlign && isRead(input(Stageables().CTRL).mem_cmd)
      loadMaExceptionClient.payload.code := Causes.misaligned_load.U
      loadMaExceptionClient.payload.badAddr := io.cmd.bits.addr
      storeMaExceptionClient.valid := misAlign && isWrite(input(Stageables().CTRL).mem_cmd)
      storeMaExceptionClient.payload.code := Causes.misaligned_store.U
      storeMaExceptionClient.payload.badAddr := io.cmd.bits.addr
    }
    ScalarPipe.Stages("memAccess").outputStages.head plug {
      io.resp.ready := input(Stageables().CTRL).mem
      writeRegFileClient.valid := input(Stageables().CTRL).mem && isRead(input(Stageables().CTRL).mem_cmd)
      writeRegFileClient.payload := loadStoreLogic.padRData(loadStoreLogic.cmd, input(Stageables().IR).extBit, io.rdata.bits.data)
      loadAccessExceptionClient.valid := input(Stageables().CTRL).mem && enq.isFire && isRead(input(Stageables().CTRL).mem_cmd) && io.resp.bits.error
      loadAccessExceptionClient.payload.default.code := Causes.load_access.U

      storeAccessExceptionClient.valid := input(Stageables().CTRL).mem && enq.isFire && isWrite(input(Stageables().CTRL).mem_cmd) && io.resp.bits.error
      storeAccessExceptionClient.payload.default.code := Causes.store_access.U
      when(loadStoreLogic.waiting) {
        enq.ready := false.B
      }
    }
  }
}
