package genes.backbone

import chisel3._
import chisel3.iotesters._

import scala.util.control.Breaks._

class pipelineInIO extends Bundle {
  val valid = Bool()
  val a = UInt(32.W)
  val b = UInt(32.W)
}

class Add extends MultiIOModule {
  val io = IO(new Bundle {
    val in1 = Input(UInt(32.W))
    val in2 = Input(UInt(32.W))
    val out = Output(UInt(32.W))
  })
  io.out := io.in1 + io.in2
}

class MulIO extends Bundle {
  val in = Input(UInt(32.W))
  val out = Output(Bool())
}

class Mul(val io: MulIO, stageId: String)(implicit pipeline: Pipeline) extends Plugin {

  //can omit setup
  def build(): Unit = {
    pipeline.Stages(stageId) plug {
      val a = input(A)
      val b = input(B)
      asStart(A_MUL_B) := a * b
      //outputMultiCycle(A_MUL_B, 2) := a * b
      io.out := deq.valid
      //io.out:= thisStage.channel.Deq.valid
    }
    pipeline.Stages(stageId).outputStages.head plug {
      val mul = input(A_MUL_B)
      val add = Module(new Add)
      add.io.in1 := io.in
      add.io.in2 := mul
      asStart(A_MUL_B_PLUS_A) := add.io.out
      //outputMultiCycle(A_MUL_B_PLUS_A, 1) := add.io.out
    }
  }
}


class StageInsertLogicBasic extends MultiIOModule with Pipeline {
  type T = StageInsertLogicBasic
  val io = IO(new Bundle {
    val input = Input(new pipelineInIO)
    val inputReady = Output(Bool())
    val output = Output(new Bundle {
      val plus = UInt(32.W)
      val mul = UInt(32.W)
      val mulplus = UInt(32.W)
      val mulplusValid = Output(Bool())
      val mulValid = Output(Bool())
    }
    )
  }
  )
  val stage1 = MultiCycleStage("stage1", 2, false)
  val stage2 = MultiCycleStage("stage2", 1, false)
  connection {AddStage(stage1) --> AddStage(stage2)}

  stage1.enq.valid := io.input.valid
  io.inputReady := stage1.enq.ready
  stage1.asStart(A) := io.input.a
  stage1.asStart(B) := io.input.b
  io.output.plus := stage1.output(A_PLUS_B)
  io.output.mul := stage1.output(A_MUL_B)
  io.output.mulplus := stage2.output(A_MUL_B_PLUS_A)
  io.output.mulplusValid := stage2.deq.isFire
  //io.output.mulplusValid := stage2.channel.Deq.isFire

  val addStage1 = new Plugin {
    def build(): Unit = {
      pipeline.Stages("stage1") plug {
        val add = Module(new Add)
        add.io.in1 := input(A)
        add.io.in2 := input(B)
        asStart(A_PLUS_B) := add.io.out
        //outputMultiCycle(A_PLUS_B, 3) := add.io.out
      }
    }
  }
  val mulio = Wire(new MulIO)
  mulio.in := stage2.input(A)
  io.output.mulValid := mulio.out
  val mul = new Mul(mulio, "stage1")

  AddPlugin(Seq(mul, addStage1): _*)
  build()
}

class StageInsertLogicTester(c: StageInsertLogicBasic) extends PeekPokeTester(c) {
  def driveInput = {
    poke(c.io.input.valid, 1)
    breakable {
      for (i <- 0 to 10) {
        if (peek(c.io.inputReady) == 1) {
          step(1)
          break
        }
        step(1)
      }
    }
    poke(c.io.input.valid, 0)
  }
}

