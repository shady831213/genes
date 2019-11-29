package genes.backbone

import chisel3._
import chisel3.iotesters._
import firrtl.options.OptionsException

class DAGUnitTest extends ChiselFlatSpec {
  "loop detect test" should "detect the loop" in {
    intercept[OptionsException] {
      iotesters.Driver.execute(Array("--target-dir", "test_run_dir/loop_detect_test"), () => new MultiIOModule with Pipeline {
        type T = Pipeline
        val testStages = Seq.tabulate(5)(i => AddStage(StdStage(s"stage_$i")))
        /*
        0 --> 1 --> 2 --> 3 --> 4 --> 1
        */
        connection(testStages.reduce(_ --> _) --> testStages(1))
      }) {
        c => new PeekPokeTester(c) {}

      }
    }.getCause.getMessage should be("requirement failed: loop detected:stage_1-->stage_2-->stage_3-->stage_4-->stage_1!")
  }

  "indexing test" should "in reverse topsort order" in {
    iotesters.Driver.execute(Array("--target-dir", "test_run_dir/indexing_test"), () => new MultiIOModule with Pipeline {
      type T = Pipeline
      val testStages = Seq.tabulate(9)(i => AddStage(StdStage(s"stage_$i")))
      /*
      5 --> 6 --> 4-----------
              |              |
              |              |-> 1 --> 8
              |              |
              --> 3 --> 2 - -
                    |      X
                    --> 0 - -> 7
       */
      connection {
        Stages("stage_5") --> Stages("stage_6") --> Seq(Stages("stage_4"), Stages("stage_3"))
        Seq(Stages("stage_4"), Stages("stage_2"), Stages("stage_0")) --> Stages("stage_1") --> Stages("stage_8")
        Stages("stage_3") --> Seq(Stages("stage_2"), Stages("stage_0")) --> Stages("stage_7")
      }
    }) {
      c =>
        new PeekPokeTester(c) {
          val pipeline = c.asInstanceOf[Pipeline]
          assert(c.Stages("stage_0").index == 5)
          assert(c.Stages("stage_1").index == 6)
          assert(c.Stages("stage_2").index == 4)
          assert(c.Stages("stage_3").index == 3)
          assert(c.Stages("stage_4").index == 2)
          assert(c.Stages("stage_5").index == 0)
          assert(c.Stages("stage_6").index == 1)
          assert(c.Stages("stage_7").index == 8)
          assert(c.Stages("stage_8").index == 7)
        }

    } should be(true)
  }

  "redefine test" should "checked the asStart redefine" in {
    intercept[OptionsException] {
      iotesters.Driver.execute(Array("--target-dir", "test_run_dir/redefine_test"), () => new MultiIOModule with Pipeline {
        /*
          0 --> 1 --> 2 --> 3 --> 4
          |                       |
        asStart(A)              asStart(A)
         */
        val testStages = Seq.tabulate(5)(i => AddStage(StdStage(s"stage_$i")))
        connection(testStages.reduce(_ --> _))
        testStages.head.asStart(A) := 0.U
        testStages.last.asStart(A) := 3.U
        build()
      }) {
        c => new PeekPokeTester(c) {}

      }
    }.getCause.getMessage should be("requirement failed: asStart A redefined! startStages:stage_0,stage_4")
  }

  "ref_before_def test" should "checked input/output reference before asStart" in {
    intercept[OptionsException] {
      iotesters.Driver.execute(Array("--target-dir", "test_run_dir/ref_before_def_test"), () => new MultiIOModule with Pipeline {
        type T = Pipeline
        /*
          0 --> 1 --> 2 --> 3 --> 4
          |                 |     |
        asStart(A)       output(B)|
                                 input(B)
         */
        val testStages = Seq.tabulate(5)(i => AddStage(StdStage(s"stage_$i")))
        connection(testStages.reduce(_ --> _))
        testStages.head.asStart(A) := 0.U
        Stages("stage_3").output(B)
        Stages("stage_4").input(B)
        build()
      }) {
        c => new PeekPokeTester(c) {}

      }
    }.getCause.getMessage should be("requirement failed: output B in stage stage_3 is referred before declare asStart()!")
    intercept[OptionsException] {
      iotesters.Driver.execute(Array("--target-dir", "test_run_dir/ref_before_def1_test"), () => new MultiIOModule with Pipeline {
        type T = Pipeline
        /*
          0 --> 1 --> 2 --> 3 --> 4
          |                       |
        asStart(A)                |
                                 input(B)
         */
        val testStages = Seq.tabulate(5)(i => AddStage(StdStage(s"stage_$i")))
        connection(testStages.reduce(_ --> _))
        testStages.head.asStart(A) := 0.U
        Stages("stage_4").input(B)
        build()
      }) {
        c => new PeekPokeTester(c) {}

      }
    }.getCause.getMessage should be("requirement failed: input B in stage stage_4 is referred before declare asStart()!")
  }

  "simple_build test" should "build pass" in {
    class dut extends MultiIOModule with Pipeline {
      val testStages = Seq.tabulate(6)(i => AddStage(StdStage(s"stage_$i")))
      /*
        0 --> 1 --> 2
              |
              --> 3 --> 4 --> 5
                   0      1      2      3      4      5
        inputs    {}     {}     {}     {}      {B}    {}
        outputs   {}     {}     {C}    {A}     {}     {}
        starts    {A}    {B,C,D}{}     {}      {}     {}
       */
      connection {
        Stages("stage_0")--> Stages("stage_1") --> Seq(Stages("stage_2"), Stages("stage_3"))
        Stages("stage_3") --> Stages("stage_4") --> Stages("stage_5")
      }

      Stages("stage_0").asStart(A) := 0.U
      Stages("stage_1").asStart(B) := 0.U
      Stages("stage_1").asStart(C) := 0.U
      Stages("stage_1").asStart(D) := 0.U
      Stages("stage_2").output(C)
      Stages("stage_3").output(A)
      Stages("stage_4").input(B)

      val table = InsertStageablePass(this, Set[Stageable[Data]]())()
      ConnectPass(this)()

    }
    iotesters.Driver.execute(Array("--target-dir", "test_run_dir/simple_build_test"), () => new dut) {
      c =>
        new PeekPokeTester(c) {
          assert(c.table.activeInputFaninSetTable(0).equals(Set()))
          assert(c.table.activeOutputFaninSetTable(0).equals(Set()))
          assert(c.table.activeFanoutSetTable(0).equals(Set(A)))
          assert(c.table.activeInputFaninSetTable(1).equals(Set(A)))
          assert(c.table.activeOutputFaninSetTable(1).equals(Set(A)))
          assert(c.table.activeFanoutSetTable(1).equals(Set(A, B, C)))
          assert(c.table.activeInputFaninSetTable(2).equals(Set(C)))
          assert(c.table.activeOutputFaninSetTable(2).equals(Set(C)))
          assert(c.table.activeFanoutSetTable(2).equals(Set()))
          assert(c.table.activeInputFaninSetTable(3).equals(Set(A, B)))
          assert(c.table.activeOutputFaninSetTable(3).equals(Set(A, B)))
          assert(c.table.activeFanoutSetTable(3).equals(Set(B)))
          assert(c.table.activeInputFaninSetTable(4).equals(Set(B)))
          assert(c.table.activeOutputFaninSetTable(4).equals(Set()))
          assert(c.table.activeFanoutSetTable(4).equals(Set()))
          assert(c.table.activeInputFaninSetTable(5).equals(Set()))
          assert(c.table.activeOutputFaninSetTable(5).equals(Set()))
          assert(c.table.activeFanoutSetTable(5).equals(Set()))
        }
    } should be(true)
  }

  "multiStage_build test" should "build pass" in {
    class dut extends MultiIOModule with Pipeline {

      val testStages = AddStage(StdStage(s"stage_0")) :: AddStage(MultiCycleStage("stage_1", 3)) :: Nil ++ Seq.tabulate(4)(i => AddStage(StdStage(s"stage_${i + 2}")))
      /*
        0 --> 1 --> 2
              |
              --> 3 --> 4 --> 5
                   0      1                              2      3      4      5
                                1_0      1_1      1_2
        inputs    {}     {}     {A}      {}       {}     {}     {}      {B}    {}
        outputs   {}     {}     {}       {D}      {}     {C}    {A}     {}     {}
        starts    {A}    {B,D}  {}       {C}      {}     {}     {}      {}     {}
       */
      connection {
        Stages("stage_0")--> Stages("stage_1") --> Seq(Stages("stage_2"), Stages("stage_3"))
        Stages("stage_3") --> Stages("stage_4") --> Stages("stage_5")
      }

      Stages("stage_0").asStart(A) := 0.U
      Stages("stage_1").asStart(B) := 0.U
      Stages("stage_1").asInstanceOf[PipeDAG].Stages("stage_1_0").input(A)
      Stages("stage_1").asInstanceOf[PipeDAG].Stages("stage_1_1").asStart(C) := 0.U
      Stages("stage_1").asStart(D) := 0.U
      Stages("stage_1").asInstanceOf[PipeDAG].Stages("stage_1_1").output(D)
      Stages("stage_2").output(C)
      Stages("stage_3").output(A)
      Stages("stage_4").input(B)

      val table = InsertStageablePass(this, Set[Stageable[Data]]())()

      ConnectPass(this)()
    }
    iotesters.Driver.execute(Array("--target-dir", "test_run_dir/multiStage_build_test"), () => new dut) {
      c =>
        new PeekPokeTester(c) {
          assert(c.table.activeInputFaninSetTable(0).equals(Set()))
          assert(c.table.activeOutputFaninSetTable(0).equals(Set()))
          assert(c.table.activeFanoutSetTable(0).equals(Set(A)))
          assert(c.table.activeInputFaninSetTable(1).equals(Set(A)))
          assert(c.table.activeOutputFaninSetTable(1).equals(Set(A)))
          assert(c.table.activeFanoutSetTable(1).equals(Set(A, B, C)))
          assert(c.table.activeInputFaninSetTable(2).equals(Set(C)))
          assert(c.table.activeOutputFaninSetTable(2).equals(Set(C)))
          assert(c.table.activeFanoutSetTable(2).equals(Set()))
          assert(c.table.activeInputFaninSetTable(3).equals(Set(A, B)))
          assert(c.table.activeOutputFaninSetTable(3).equals(Set(A, B)))
          assert(c.table.activeFanoutSetTable(3).equals(Set(B)))
          assert(c.table.activeInputFaninSetTable(4).equals(Set(B)))
          assert(c.table.activeOutputFaninSetTable(4).equals(Set()))
          assert(c.table.activeFanoutSetTable(4).equals(Set()))
          assert(c.table.activeInputFaninSetTable(5).equals(Set()))
          assert(c.table.activeOutputFaninSetTable(5).equals(Set()))
          assert(c.table.activeFanoutSetTable(5).equals(Set()))
        }
    } should be(true)
  }
}
