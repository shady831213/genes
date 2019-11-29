package genes.example.test.GenesExample

import java.io.File
import java.lang.Long

import genes.example.design.GenesExample.GenesExampleForTest
import genes.example.design.GenesExample.config._
import genes.example.design.GenesExample.modules.loadStore.{LoadStoreCmdCh, LoadStoreIO}
import genes.organs.rocket.config._
import genes.organs.tester.memory._
import genes.organs.tester.testers.MultiThreadsTester
import chisel3._
import chisel3.iotesters.ChiselFlatSpec
import chisel3.util._
import scala.collection.concurrent.TrieMap

abstract class GETestConfig {
  implicit val p: Parameters
}

trait WithHexMemory extends WithMemory {
  def loadMem(file: String): Seq[BigInt] = {
    val stream = getClass.getResource(s"/hex/${file}.hex")
    io.Source.fromURL(stream).getLines.toSeq.map { s => BigInt(Long.parseUnsignedLong(s, 16)) }
  }
}

class GETester[T <: GETB](c: T)(implicit val p: Parameters) extends MultiThreadsTester(c) with GEParams with WithHexMemory {
  def imemReadyDriver(): userThread = {
    randomReady(c.io.fetchIO.req.valid, c.io.fetchIO.resp.valid)
  }

  def randomReady(req: Bool, ready: Bool): userThread = {
    new userThread {
      def job: Unit = {
        while (true) {
          if (peek(req) == 1) {
            var resp = scala.util.Random.nextBoolean()
            while (!resp) {
              poke(ready, resp)
              waitUntil()
              resp = scala.util.Random.nextBoolean()
            }
            poke(ready, resp)
          }
          waitUntil()
        }
      }
    }
  }

  def dmemReadyDriver(): userThread = {
    //    new userThread {
    //      def job: Unit = {
    //        poke(c.io.loadStoreIO.resp.valid, 1)
    //      }
    //    }
    randomReady(c.io.loadStoreIO.cmd.valid, c.io.loadStoreIO.resp.valid)
  }
}


class GETB(implicit p: Parameters) extends GEMultiIOModule()(p) {
  val dut = Module(new GenesExampleForTest)
  val io = IO(dut.io.cloneType)
  val testIO = IO(dut.testIO.cloneType)
  val imemModel = Module(if (fetchWidth == 2) new DWORDMEMSWAR(addrWidth = xlen) else new WORDMEMSWAR(addrWidth = xlen))
  val dmemModel = Module({
    if (xlen == 64) new DWORDMEMSWSR(addrWidth = xlen) else new WORDMEMSWSR(addrWidth = xlen)
  })
  io <> dut.io
  testIO <> dut.testIO

  imemModel.io.clock := clock
  imemModel.io.reset := reset
  imemModel.io.we := false.B
  imemModel.io.waddr := 0.U
  imemModel.io.wdata := 0.U
  imemModel.io.wmask := 0.U
  imemModel.io.raddr := (dut.io.fetchIO.req.bits.pc >> log2Ceil((32 * fetchWidth) / 8)).asUInt
  for (i <- 0 until fetchWidth) {
    dut.io.fetchIO.resp.bits.insts(i) := imemModel.io.rdata((i + 1) * 32 - 1, i * 32)
  }
  dut.io.fetchIO.resp.bits.error := imemModel.io.err

  dmemModel.io.clock := clock
  dmemModel.io.reset := reset
  dmemModel.io.we := io.loadStoreIO.cmd.valid && io.loadStoreIO.cmd.bits.wr
  dmemModel.io.waddr := (io.loadStoreIO.cmd.bits.addr >> log2Ceil(xlen / 8)).asUInt
  dmemModel.io.wdata := WireInit(io.loadStoreIO.wdata.bits.data).suggestName("loadStore_cmd_wdata")
  dmemModel.io.wmask := WireInit(io.loadStoreIO.wdata.bits.mask).suggestName("loadStore_cmd_wmask")
  dmemModel.io.raddr := (io.loadStoreIO.cmd.bits.addr >> log2Ceil(xlen / 8)).asUInt
  dmemModel.io.re := io.loadStoreIO.cmd.valid && !io.loadStoreIO.cmd.bits.wr
  dut.io.loadStoreIO.rdata.bits.data := dmemModel.io.rdata
  dut.io.loadStoreIO.rdata.bits.last := true.B
  dut.io.loadStoreIO.cmd.ready := true.B
  dut.io.loadStoreIO.resp.bits.error := dmemModel.io.err
}

class GEFlatSpec(val config: GETestConfig) extends ChiselFlatSpec {

  import config._

  def defaultConfig = Array("")

  private val dutInst = TrieMap[Boolean, GETB]()
  private val getDut =  () => dutInst.getOrElseUpdate(true, new GETB())


  def riscvISATest(name: String, fileName: String): Unit = {
    val mem = if (p(XLEN) == 64) new BevMemory(0, BigInt(0xfffffffffffffffL), BigInt(0xffffffffffffffffL)) else new BevMemory(0, BigInt(0xffffffffL), BigInt(0xffffffffL))
    val testTopName = String.join("_", name.split(" "): _*)
    val optionsManager = new TesterOptionsManagerWithMemory(mem)
    optionsManager.parse(defaultConfig ++ Array("--target-dir", s"test_run_dir/$testTopName",
      "--top-name", testTopName))
    iotesters.Driver.execute(getDut, optionsManager) {
      c =>
        new GETester(c) {
          val startAddr = 0x80000000L
          val toHost = startAddr + 0x1000L
          val datas = loadMem(fileName)
          memInit(datas, mem, startAddr)
          val iter = datas.iterator
          val seq = new userThread {
            def job: Unit = {
              while (mem.Read(toHost) == 0) {
                waitUntil()
              }
              val result = peek(c.testIO.regFile.gp)
              if (result != 1) {
                println(s"$name: test ${result >> 1} failed!")
                fail
              }
            }
          }.Start()
          imemReadyDriver().Start()
          dmemReadyDriver().Start()
          start(5000)
          seq.join()
        }
    } should be(true)
  }
}

trait WithDebug extends GEFlatSpec {
  override def defaultConfig = super.defaultConfig ++ Array("--is-verbose", "--generate-vcd-output", "on", "--tr-vcd-show-underscored-vars" /*, "-tts", "1560474408327"*/)
}

trait GE32Tests extends GEFlatSpec {
  "simple test " should "pass rv32ui-p-simple" in riscvISATest("simple test", "rv32ui-p-simple")
  "add test " should "pass rv32ui-p-add" in riscvISATest("add test", "rv32ui-p-add")
  "addi test " should "pass rv32ui-p-addi" in riscvISATest("addi test", "rv32ui-p-addi")
  "and test " should "pass rv32ui-p-and" in riscvISATest("and test", "rv32ui-p-and")
  "andi test " should "pass rv32ui-p-andi" in riscvISATest("andi test", "rv32ui-p-andi")
  "auipc test " should "pass rv32ui-p-auipc" in riscvISATest("auipc test", "rv32ui-p-auipc")
  "beq test " should "pass rv32ui-p-beq" in riscvISATest("beq test", "rv32ui-p-beq")
  "bge test " should "pass rv32ui-p-bge" in riscvISATest("bge test", "rv32ui-p-bge")
  "bgeu test " should "pass rv32ui-p-bgeu" in riscvISATest("bgeu test", "rv32ui-p-bgeu")
  "blt test " should "pass rv32ui-p-blt" in riscvISATest("blt test", "rv32ui-p-blt")
  "bltu test " should "pass rv32ui-p-bltu" in riscvISATest("bltu test", "rv32ui-p-bltu")
  "bne test " should "pass rv32ui-p-bne" in riscvISATest("bne test", "rv32ui-p-bne")
  "fence_i test " should "pass rv32ui-p-fence_i" in riscvISATest("fence_i test", "rv32ui-p-fence_i")
  "jal test " should "pass rv32ui-p-jal" in riscvISATest("jal test", "rv32ui-p-jal")
  "jalr test " should "pass rv32ui-p-jalr" in riscvISATest("jalr test", "rv32ui-p-jalr")
  "lb test " should "pass rv32ui-p-lb" in riscvISATest("lb test", "rv32ui-p-lb")
  "lbu test " should "pass rv32ui-p-lbu" in riscvISATest("lbu test", "rv32ui-p-lbu")
  "lh test " should "pass rv32ui-p-lh" in riscvISATest("lh test", "rv32ui-p-lh")
  "lhu test " should "pass rv32ui-p-lhu" in riscvISATest("lhu test", "rv32ui-p-lhu")
  "lui test " should "pass rv32ui-p-lui" in riscvISATest("lui test", "rv32ui-p-lui")
  "lw test " should "pass rv32ui-p-lw" in riscvISATest("lw test", "rv32ui-p-lw")
  "or test " should "pass rv32ui-p-or" in riscvISATest("or test", "rv32ui-p-or")
  "ori test " should "pass rv32ui-p-ori" in riscvISATest("ori test", "rv32ui-p-ori")
  "sb test " should "pass rv32ui-p-sb" in riscvISATest("sb test", "rv32ui-p-sb")
  "sh test " should "pass rv32ui-p-sh" in riscvISATest("sh test", "rv32ui-p-sh")
  "sll test " should "pass rv32ui-p-sll" in riscvISATest("sll test", "rv32ui-p-sll")
  "slli test " should "pass rv32ui-p-slli" in riscvISATest("slli test", "rv32ui-p-slli")
  "slt test " should "pass rv32ui-p-slt" in riscvISATest("slt test", "rv32ui-p-slt")
  "slti test " should "pass rv32ui-p-slti" in riscvISATest("slti test", "rv32ui-p-slti")
  "sltiu test " should "pass rv32ui-p-sltiu" in riscvISATest("sltiu test", "rv32ui-p-sltiu")
  "sltu test " should "pass rv32ui-p-sltu" in riscvISATest("sltu test", "rv32ui-p-sltu")
  "sra test " should "pass rv32ui-p-sra" in riscvISATest("sra test", "rv32ui-p-sra")
  "srai test " should "pass rv32ui-p-srai" in riscvISATest("srai test", "rv32ui-p-srai")
  "srl test " should "pass rv32ui-p-srl" in riscvISATest("srl test", "rv32ui-p-srl")
  "srli test " should "pass rv32ui-p-srli" in riscvISATest("srli test", "rv32ui-p-srli")
  "sub test " should "pass rv32ui-p-sub" in riscvISATest("sub test", "rv32ui-p-sub")
  "sw test " should "pass rv32ui-p-sw" in riscvISATest("sw test", "rv32ui-p-sw")
  "xor test " should "pass rv32ui-p-xor" in riscvISATest("xor test", "rv32ui-p-xor")
  "xori test " should "pass rv32ui-p-xori" in riscvISATest("xori test", "rv32ui-p-xori")

  //rv32mi-p
  "illegal test " should "pass rv32mi-p-illegal" in riscvISATest("illegal test", "rv32mi-p-illegal")
  "scall test " should "pass rv32mi-p-scall" in riscvISATest("scall test", "rv32mi-p-scall")
  "ma_fetch test " should "pass rv32mi-p-ma_fetch" in riscvISATest("ma_fetch test", "rv32mi-p-ma_fetch")
  "ma_addr test " should "pass rv32mi-p-ma_addr" in riscvISATest("ma_addr test", "rv32mi-p-ma_addr")
  "csr test " should "pass rv32mi-p-csr" in riscvISATest("csr test", "rv32mi-p-csr")
  "mcsr test " should "pass rv32mi-p-mcsr" in riscvISATest("mcsr test", "rv32mi-p-mcsr")
  "sbreak test " should "pass rv32mi-p-sbreak" in riscvISATest("sbreak test", "rv32mi-p-sbreak")
  "breakpoint test " should "pass rv32mi-p-breakpoint" in riscvISATest("breakpoint test", "rv32mi-p-breakpoint")
  //rv32mi-p-only
  "shamt test " should "pass rv32mi-p-shamt" in riscvISATest("shamt test", "rv32mi-p-shamt")
}

trait GE64Tests extends GEFlatSpec {
  //rv64ui-p
  //64/32ui-p
  "simple 64 test " should "pass rv64ui-p-simple" in riscvISATest("simple 64 test", "rv64ui-p-simple")
  "add 64 test " should "pass rv64ui-p-add" in riscvISATest("add 64 test", "rv64ui-p-add")
  "addi 64 test " should "pass rv64ui-p-addi" in riscvISATest("addi 64 test", "rv64ui-p-addi")
  "and 64 test " should "pass rv64ui-p-and" in riscvISATest("and 64 test", "rv64ui-p-and")
  "andi 64 test " should "pass rv64ui-p-andi" in riscvISATest("andi 64 test", "rv64ui-p-andi")
  "auipc 64 test " should "pass rv64ui-p-auipc" in riscvISATest("auipc 64 test", "rv64ui-p-auipc")
  "beq 64 test " should "pass rv64ui-p-beq" in riscvISATest("beq 64 test", "rv64ui-p-beq")
  "bge 64 test " should "pass rv64ui-p-bge" in riscvISATest("bge 64 test", "rv64ui-p-bge")
  "bgeu 64 test " should "pass rv64ui-p-bgeu" in riscvISATest("bgeu 64 test", "rv64ui-p-bgeu")
  "blt 64 test " should "pass rv64ui-p-blt" in riscvISATest("blt 64 test", "rv64ui-p-blt")
  "bltu 64 test " should "pass rv64ui-p-bltu" in riscvISATest("bltu 64 test", "rv64ui-p-bltu")
  "bne 64 test " should "pass rv64ui-p-bne" in riscvISATest("bne 64 test", "rv64ui-p-bne")
  "fence_i 64 test " should "pass rv64ui-p-fence_i" in riscvISATest("fence_i 64 test", "rv64ui-p-fence_i")
  "jal 64 test " should "pass rv64ui-p-jal" in riscvISATest("jal 64 test", "rv64ui-p-jal")
  "jalr 64 test " should "pass rv64ui-p-jalr" in riscvISATest("jalr 64 test", "rv64ui-p-jalr")
  "lb 64 test " should "pass rv64ui-p-lb" in riscvISATest("lb 64 test", "rv64ui-p-lb")
  "lbu 64 test " should "pass rv64ui-p-lbu" in riscvISATest("lbu 64 test", "rv64ui-p-lbu")
  "lh 64 test " should "pass rv64ui-p-lh" in riscvISATest("lh 64 test", "rv64ui-p-lh")
  "lhu 64 test " should "pass rv64ui-p-lhu" in riscvISATest("lhu 64 test", "rv64ui-p-lhu")
  "lui 64 test " should "pass rv64ui-p-lui" in riscvISATest("lui 64 test", "rv64ui-p-lui")
  "lw 64 test " should "pass rv64ui-p-lw" in riscvISATest("lw 64 test", "rv64ui-p-lw")
  "or 64 test " should "pass rv64ui-p-or" in riscvISATest("or 64 test", "rv64ui-p-or")
  "ori 64 test " should "pass rv64ui-p-ori" in riscvISATest("ori 64 test", "rv64ui-p-ori")
  "sb 64 test " should "pass rv64ui-p-sb" in riscvISATest("sb 64 test", "rv64ui-p-sb")
  "sh 64 test " should "pass rv64ui-p-sh" in riscvISATest("sh 64 test", "rv64ui-p-sh")
  "sll 64 test " should "pass rv64ui-p-sll" in riscvISATest("sll 64 test", "rv64ui-p-sll")
  "slli 64 test " should "pass rv64ui-p-slli" in riscvISATest("slli 64 test", "rv64ui-p-slli")
  "slt 64 test " should "pass rv64ui-p-slt" in riscvISATest("slt 64 test", "rv64ui-p-slt")
  "slti 64 test " should "pass rv64ui-p-slti" in riscvISATest("slti 64 test", "rv64ui-p-slti")
  "sltiu 64 test " should "pass rv64ui-p-sltiu" in riscvISATest("sltiu 64 test", "rv64ui-p-sltiu")
  "sltu 64 test " should "pass rv64ui-p-sltu" in riscvISATest("sltu 64 test", "rv64ui-p-sltu")
  "sra 64 test " should "pass rv64ui-p-sra" in riscvISATest("sra 64 test", "rv64ui-p-sra")
  "srai 64 test " should "pass rv64ui-p-srai" in riscvISATest("srai 64 test", "rv64ui-p-srai")
  "srl 64 test " should "pass rv64ui-p-srl" in riscvISATest("srl 64 test", "rv64ui-p-srl")
  "srli 64 test " should "pass rv64ui-p-srli" in riscvISATest("srli 64 test", "rv64ui-p-srli")
  "sub 64 test " should "pass rv64ui-p-sub" in riscvISATest("sub 64 test", "rv64ui-p-sub")
  "sw 64 test " should "pass rv64ui-p-sw" in riscvISATest("sw 64 test", "rv64ui-p-sw")
  "xor 64 test " should "pass rv64ui-p-xor" in riscvISATest("xor 64 test", "rv64ui-p-xor")
  "xori 64 test " should "pass rv64ui-p-xori" in riscvISATest("xori 64 test", "rv64ui-p-xori")
  //64ui-p-only
  "addiw 64 test " should "pass rv64ui-p-addiw" in riscvISATest("addiw 64 test", "rv64ui-p-addiw")
  "addw 64 test " should "pass rv64ui-p-addw" in riscvISATest("addw 64 test", "rv64ui-p-addw")
  "ld 64 test " should "pass rv64ui-p-ld" in riscvISATest("ld 64 test", "rv64ui-p-ld")
  "lwu 64 test " should "pass rv64ui-p-lwu" in riscvISATest("lwu 64 test", "rv64ui-p-lwu")
  "sd 64 test " should "pass rv64ui-p-sd" in riscvISATest("sd 64 test", "rv64ui-p-sd")
  "slliw 64 test " should "pass rv64ui-p-slliw" in riscvISATest("slliw 64 test", "rv64ui-p-slliw")
  "sllw 64 test " should "pass rv64ui-p-sllw" in riscvISATest("sllw 64 test", "rv64ui-p-sllw")
  "sraiw 64 test " should "pass rv64ui-p-sraiw" in riscvISATest("sraiw 64 test", "rv64ui-p-sraiw")
  "sraw 64 test " should "pass rv64ui-p-sraw" in riscvISATest("sraw 64 test", "rv64ui-p-sraw")
  "srliw 64 test " should "pass rv64ui-p-srliw" in riscvISATest("srliw 64 test", "rv64ui-p-srliw")
  "srlw 64 test " should "pass rv64ui-p-srlw" in riscvISATest("srlw 64 test", "rv64ui-p-srlw")
  "subw 64 test " should "pass rv64ui-p-subw" in riscvISATest("subw 64 test", "rv64ui-p-subw")

  //rv64mi-p
  //rv64/32mi-p
  "illegal 64 test " should "pass rv64mi-p-illegal" in riscvISATest("illegal 64 test", "rv64mi-p-illegal")
  "scall 64 test " should "pass rv64mi-p-scall" in riscvISATest("scall 64 test", "rv64mi-p-scall")
  "ma_fetch 64 test " should "pass rv64mi-p-ma_fetch" in riscvISATest("ma_fetch 64 test", "rv64mi-p-ma_fetch")
  "ma_addr 64 test " should "pass rv64mi-p-ma_addr" in riscvISATest("ma_addr 64 test", "rv64mi-p-ma_addr")
  "csr 64 test " should "pass rv64mi-p-csr" in riscvISATest("csr 64 test", "rv64mi-p-csr")
  "mcsr 64 test " should "pass rv64mi-p-mcsr" in riscvISATest("mcsr 64 test", "rv64mi-p-mcsr")
  "sbreak 64 test " should "pass rv64mi-p-sbreak" in riscvISATest("sbreak 64 test", "rv64mi-p-sbreak")
  "breakpoint 64 test " should "pass rv64mi-p-breakpoint" in riscvISATest("breakpoint 64 test", "rv64mi-p-breakpoint")
  //rv64mi-p only
  "access 64 test " should "pass rv64mi-p-access" in riscvISATest("access 64 test", "rv64mi-p-access")

}
