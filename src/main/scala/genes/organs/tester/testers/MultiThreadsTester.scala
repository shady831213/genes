package genes.organs.tester.testers

import java.io.File
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.function.Consumer

import chisel3.core.{Aggregate, MultiIOModule}
import chisel3.iotesters.{PeekPokeTester, Pokeable}
import chisel3.{Bundle, Element}

import scala.concurrent.SyncVar
import scala.util.control.Breaks.{break, breakable}

class MultiThreadsTester[+T <: MultiIOModule](dut: T,
                                              base: Int = 16,
                                              logFile: Option[File] = None) extends PeekPokeTester(dut) {
  private val threads = new ConcurrentLinkedQueue[userThread]()
  private var daemon: timeline = null
  private val testFinish = new SyncVar[Boolean]

  private class timeline(timeout: Long = 0) extends Thread {
    override def run(): Unit = {
      println("timeout start!")
      var timeCnt: Long = 0
      breakable {
        while (true) {
          step(1)
          threads.forEach(new Consumer[userThread] {
            def accept(thread: userThread): Unit = {
              thread.sync.put(t)
              thread.done.take()
            }
          })
          if (testFinish.isSet) {
            threads.forEach(new Consumer[userThread] {
              def accept(thread: userThread): Unit = {
                thread.stop()
              }
            })
            break()
          }
          timeCnt += 1
          if (timeCnt >= timeout && timeout > 0) {
            println("timeout!")
            fail
            threads.forEach(new Consumer[userThread] {
              def accept(thread: userThread): Unit = {
                thread.stop()
              }
            })
            break()
          }
        }
      }
    }
  }

  abstract class userThread extends Thread {
    val sync = new SyncVar[Long]
    val done = new SyncVar[Boolean]

    def job: Unit

    def Start(): userThread = {
      threads.add(this)
      start()
      this
    }

    private def waitCycles(n: Long = 1) = {
      require(n > 0)
      for (i <- 0 until n) {
        done.put(true)
        sync.take()
      }
    }

    def peekReg(path: String): BigInt = {
      waitCycles()
      peek(path)
    }

    def peekReg[T <: Element : Pokeable](signal: T): BigInt = {
      waitCycles()
      peek(signal)
    }

    def peekReg(signal: Aggregate): Seq[BigInt] = {
      waitCycles()
      peek(signal)
    }

    def expectReg(signal: Bundle, expected: Map[String, BigInt]): Boolean = {
      waitCycles()
      expect(signal, expected)
    }

    def expectReg[T <: Element : Pokeable](signal: T, expected: Int, msg: => String): Boolean = {
      waitCycles()
      expect(signal, expected, msg)
    }


    def expectReg(signal: Aggregate, expected: IndexedSeq[BigInt]): Boolean = {
      waitCycles()
      expect(signal, expected)
    }

    def expectReg[T <: Element : Pokeable](signal: T, expected: BigInt, msg: => String = ""): Boolean = {
      waitCycles()
      expect(signal, expected, msg)
    }

    def expectReg(good: Boolean, msg: => String): Boolean = {
      waitCycles()
      expect(good, msg)
    }

    def waitUntil(cycles: Long = 1): Unit = {
      require(cycles > 0)
      waitCycles(cycles)
    }

    def waitUntil(cond: => Boolean): Unit = {
      while (!cond) {
        waitCycles()
      }
    }

    final override def run(): Unit = {
      sync.take()
      job
      done.put(true)
      threads.remove(this)
    }
  }

  protected def start(timeout: Long = 0): Unit = {
    require(daemon == null)
    daemon = new timeline(timeout)
    daemon.start()
  }

  override def finish(): Boolean = {
    testFinish.put(true)
    super.finish
  }

}




