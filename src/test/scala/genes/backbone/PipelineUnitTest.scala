package genes.backbone

import chisel3._
import chisel3.iotesters.ChiselFlatSpec


class PipelineUnitTest extends ChiselFlatSpec {
  "stage insert logic test " should "insert logic to stage1" in {
    iotesters.Driver.execute(Array("--is-verbose", "--generate-vcd-output", "on", "--target-dir", "test_run_dir/stage_insert_logic_test",
      "--top-name", "stage_insert_logic_test", "--tr-vcd-show-underscored-vars"), () => new StageInsertLogicBasic) {
      c =>
        new StageInsertLogicTester(c) {
          step(1)
          println(Driver.emitVerilog(new StageInsertLogicBasic))
          poke(c.io.input.a, 3)
          poke(c.io.input.b, 2)
          driveInput
          step(1)
          expect(c.io.output.mulValid, 1)
          expect(c.io.output.plus, 5)
          expect(c.io.output.mul, 6)
          expect(c.io.output.mulplus, 0)
          step(1)
          expect(c.io.output.mulValid, 0)
          expect(c.io.output.mulplusValid, 1)
          expect(c.io.output.plus, 5)
          expect(c.io.output.mul, 6)
          expect(c.io.output.mulplus, 9)
          step(1)
          expect(c.io.output.mulplusValid, 0)

          step(3)
        }
    } should be(true)
  }

  "complex pipeline test " should "pass" in {
    iotesters.Driver.execute(Array("--is-verbose", "--generate-vcd-output", "on", "--target-dir", "test_run_dir/complex_pipeline_test",
      "--top-name", "complex_pipeline_test", "--tr-vcd-show-underscored-vars"), () => new ComplexPipelineModule) {
      c =>
        new ComplexPipelineTester(c) {
          step(1)
          val drive = new userThread {
            def job: Unit = {
              poke(c.io.fetch.data.bits, 0x55)
              driveInput(this)
              poke(c.io.fetch.data.bits, 0x55aa)
              driveInput(this)
              poke(c.io.fetch.data.bits, 0xaa55)
              driveInput(this)
              poke(c.io.fetch.data.bits, 0xaa)
              driveInput(this)
              poke(c.io.fetch.data.bits, 0xaaaa)
              driveInput(this)
              poke(c.io.fetch.data.bits, 0xaaaa)
              driveInput(this)
              poke(c.io.fetch.data.bits, 0x5555)
              driveInput(this)
              poke(c.io.fetch.data.bits, 0xaa55)
              driveInput(this)
              poke(c.io.fetch.data.bits, 0x5555)
              driveInput(this)
              poke(c.io.fetch.data.bits, 0x55aa)
              driveInput(this)
              poke(c.io.fetch.data.bits, 0x55)
              driveInput(this)
              waitUntil()
              poke(c.io.fetch.data.bits, 0x0)
              driveInput(this)
              poke(c.io.fetch.data.bits, 0xaa55)
              driveInput(this)
              poke(c.io.fetch.data.bits, 0x55)
              driveInput(this)
              waitUntil(50)
            }
          }.Start()
          var intCnt = 0
          val intMonitor = new userThread {
            def job: Unit = {
              while (true) {
                if (peek(c.io.int_output.data.valid) == 1) {
                  expect(c.io.int_output.data.bits, 0x55)
                  intCnt += 1
                }
                waitUntil()
              }
            }
          }.Start()
          var memCnt = 0
          val memMonitor = new userThread {
            def job: Unit = {
              while (true) {
                if (peek(c.io.mem_output.data.valid) == 1) {
                  expect(c.io.mem_output.data.bits, 0xaa)
                  memCnt += 1
                }
                waitUntil()
              }
            }
          }.Start()
          start(100)
          drive.join()
          expect(intCnt == 12, s"expect 12 int trans but get $intCnt")
          expect(memCnt == 10, s"expect 10 mem trans but get $memCnt")

        }
    } should be(true)
  }
}
