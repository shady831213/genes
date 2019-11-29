package genes.bones

import genes.organs.tester.testers.MultiThreadsTester
import chisel3._
import chisel3.util._

import scala.util.control.Breaks.{break, breakable}

class outputIO extends Bundle {
  val data = ValidIO(UInt(32.W))
  val enqValid = Output(Bool())
  val execute2Data = ValidIO(UInt(32.W))
}

class IntIssue extends MultiChannelStage("int_issue", Some(ch => StdStage(s"int_issue_ch$ch", ch)), 2) {
  def enqRoute(): Unit = {
    inputStages.zipWithIndex.foreach { case (stage, id) => {
      arbitInput(Seq(stage), enqs(id))
    }
    }
  }

  def deqRoute(): Unit = {
    arbitOutput(deqs, outputStages.head)
  }
}

class IntPipe extends MultiStageSingleChannelStage("int_pipe") {
  protected def subPipe(): Unit = {
    connection {
      AddStage(StdStage("int_issue")) --> AddStage(MultiCycleStage("int_execute", 3)) --> AddStage(RegStage("int_writeBack"))
    }
  }
}

class IntPipePlugin(io: outputIO)(implicit pipeline: Pipeline) extends Plugin {
  def build(): Unit = {
    val intPipe = pipeline.Stages("int_pipe").asInstanceOf[MultiStageSingleChannelStage]
    intPipe.Stages("int_issue") plug {
      val queue = Module(new Queue(UInt(32.W), 1, true, true))
      queue.io.enq.valid := enq.valid
      enq.ready := queue.io.enq.ready
      queue.io.enq.bits := input(A)
      deq.valid := queue.io.deq.valid
      queue.io.deq.ready := deq.ready
      output(A) := queue.io.deq.bits
    }

    intPipe.Stages("int_execute") plug {
      Stages(1) plug {
        asStart(B) := output(A)
      }
      Stages(2) plug {
        io.execute2Data.bits := output(A)
        io.execute2Data.valid := deq.isFire
      }
    }

    intPipe.Stages("int_writeBack") plug {
      io.data.valid := deq.isFire
      io.data.bits := output(B)
    }
    intPipe plug {
      io.enqValid := enq.valid
    }
  }
}

class MemIssue extends MultiChannelStage("mem_issue", Some(ch => StdStage(s"mem_issue_ch$ch"))) {

  def enqRoute(): Unit = {
    arbitInput(inputStages, enq)
  }

  def deqRoute(): Unit = {
    connectOutput(deq, outputStages.head)
  }
}

class MemPipe extends MultiStageSingleChannelStage("mem_pipe") {
  protected def subPipe(): Unit = {
    connection {
      AddStage(StdStage("mem_issue")) --> AddStage(MultiCycleStage("mem_execute", 3, false)) --> AddStage(RegStage("mem_memoryAccess")) --> AddStage(RegStage("mem_writeBack"))
    }
  }
}

class MemPipePlugin(io: outputIO)(implicit pipeline: Pipeline) extends Plugin {
  def build(): Unit = {
    val memPipe = pipeline.Stages("mem_pipe").asInstanceOf[MultiStageSingleChannelStage]
    memPipe.Stages("mem_issue") plug {
      val queue = Module(new Queue(UInt(32.W), 2, true, true))
      queue.io.enq.valid := enq.valid
      enq.ready := queue.io.enq.ready
      queue.io.enq.bits := input(A)
      deq.valid := queue.io.deq.valid
      queue.io.deq.ready := deq.ready
      output(A) := queue.io.deq.bits
    }

    memPipe.Stages("mem_execute") plug {
      Stages(1) plug {
        io.execute2Data.bits := output(A)
        io.execute2Data.valid := deq.isFire
      }
    }

    memPipe.Stages("mem_writeBack") plug {
      io.data.valid := deq.isFire
      io.data.bits := output(A)
    }

    memPipe plug {
      io.enqValid := enq.valid
    }
  }
}

class DecodePipe(pipeName: String) extends MultiChannelRegStage(pipeName, 2) {
  def enqRoute(): Unit = {
    broadCastInput(inputStages.head, enqs)
  }

  def deqRoute(): Unit = {
    val outputStageMap = outputStages.map(s => s.pipeName -> s).toMap
    connectOutput(deqs.head, outputStageMap("int_issue"))
    connectOutput(deqs(1), outputStageMap("mem_issue"))
  }
}


class Decoder(ch: Int)(implicit pipeline: Pipeline) extends Plugin {

  def build(): Unit = {
    pipeline.Stages(s"decode${ch}") plug {
      output(A, 0) := input(A, ch)(ch * 8 + 7, ch * 8)
      output(A, 1) := input(A, ch)(ch * 8 + 7, ch * 8)
      when(input(A, ch)(ch * 8 + 7, ch * 8) === 0x55.U) {
        deqs(0).valid := enqs(0).valid
        enqs(0).ready := deqs(0).ready
        deqs(1).valid := false.B
        enqs(1).ready := true.B
      }.elsewhen(input(A, ch)(ch * 8 + 7, ch * 8) === 0xaa.U) {
        deqs(1).valid := enqs(1).valid
        enqs(1).ready := deqs(1).ready
        deqs(0).valid := false.B
        enqs(0).ready := true.B
      }.otherwise {
        deqs(0).valid := false.B
        enqs(0).ready := true.B
        deqs(1).valid := false.B
        enqs(1).ready := true.B
      }
    }
  }
}

class QueuePipe extends MultiChannelStage("queue", Some(ch => StdStage(s"queue_ch$ch",chId = ch)), 2) {
  def enqRoute(): Unit = {
    broadCastInput(inputStages.head, enqs)
  }

  def deqRoute(): Unit = {
    deqs.zipWithIndex.foreach { case (d, id) => connectOutput(d, outputStages(id)) }
  }
}

class Adapter(implicit pipeline: Pipeline) extends Plugin {

  def build(): Unit = {
    pipeline.Stages("queue") plug {
      val decode1Process = RegInit(false.B).suggestName("decode1Process")
      when(input(A, 0)(7, 0) === input(A, 0)(15, 8) && (input(A, 0)(7, 0) === 0x55.U || input(A, 0)(7, 0) === 0xaa.U && deqs(0).isFire)) {
        decode1Process := true.B
      }
      when(decode1Process && deqs(1).isFire) {
        decode1Process := false.B
      }
      val ch0Mask = (!decode1Process || deqs(1).ready && output(A, 0)(7, 0) =/= output(A)(15, 8)) && (output(A, 0)(7, 0) === 0x55.U || output(A, 0)(7, 0) === 0xaa.U)
      val ch1Mask = (decode1Process || deqs(0).ready && output(A, 1)(7, 0) =/= output(A, 1)(15, 8)) && (output(A, 1)(15, 8) === 0x55.U || output(A, 1)(15, 8) === 0xaa.U)
      deqs(0).valid := enqs(0).valid && ch0Mask
      deqs(1).valid := enqs(1).valid && ch1Mask
      enqs(0).ready := deqs(0).ready || !ch0Mask
      enqs(1).ready := deqs(1).ready || !ch1Mask
      when(!decode1Process && input(A, 1)(7, 0) === input(A, 1)(15, 8) && (input(A)(15, 8) === 0x55.U || input(A)(15, 8) === 0xaa.U)) {
        enqs(1).ready := false.B
      }
    }
  }
}


class fetchIO extends Bundle {
  val data = Flipped(Decoupled(UInt(32.W)))
}

class Fetcher(io: fetchIO)(implicit pipeline: Pipeline) extends Plugin {
  def build(): Unit = {
    pipeline.Stages("fetch") plug {
      val queue = Module(new Queue(UInt(32.W), 1, true, false))
      queue.io.enq.valid := io.data.valid
      io.data.ready := queue.io.enq.ready
      queue.io.enq.bits := io.data.bits
      enq.valid := queue.io.deq.valid
      queue.io.deq.ready := enq.ready
      asStart(A) := queue.io.deq.bits
    }
  }
}


class ComplexPipelineModule extends MultiIOModule with Pipeline {
  val io = IO(new Bundle {
    val fetch = new fetchIO
    val int_output = new outputIO
    val mem_output = new outputIO
  })
  type T = ComplexPipelineModule

  val intPipe = AddStage(new IntPipe)
  val memPipe = AddStage(new MemPipe)
  val intIssue = AddStage(new IntIssue)
  val memIssue = AddStage(new MemIssue)
  val decodePipe = Seq.tabulate(2)(i => AddStage(new DecodePipe(s"decode$i")))


    /*
                      --> |decode_ch0|- -> int_issue --> int_pipe
    fetch --> <queue>-    |          | x
                      --> |decode_ch1|- -> mem_issue --> mem_pipe
     */
  connection {
    AddStage(StdStage("fetch")) --> AddStage(new QueuePipe) --> decodePipe --> Seq(intIssue, memIssue)
    intIssue --> intPipe
    memIssue --> memPipe
  }

  AddPlugin(new Fetcher(io.fetch), new Adapter(), new Decoder(0), new Decoder(1), new IntPipePlugin(io.int_output), new MemPipePlugin(io.mem_output))

  build()
}

class ComplexPipelineTester(c: ComplexPipelineModule) extends MultiThreadsTester(c) {
  def driveInput(thread: userThread) = {
    poke(c.io.fetch.data.valid, 1)
    breakable {
      for (i <- 0 to 10) {
        if (peek(c.io.fetch.data.ready) == 1) {
          thread.waitUntil()
          break
        }
        thread.waitUntil()
      }
    }
    poke(c.io.fetch.data.valid, 0)
  }
}