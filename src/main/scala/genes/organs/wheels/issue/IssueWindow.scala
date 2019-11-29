package genes.organs.wheels.issue

import genes.organs.wheels.buffers.RingBuffer
import chisel3._

import scala.collection.mutable

class IssueWindow(size: Int,
                  setValidFn: UInt => Unit,
                  setEntryFn: UInt => Unit,
                  clrValidFn: UInt => Unit,
                  clrEntryFn: UInt => Unit,
                  retireFn: UInt => Unit,
                  readyFn: UInt => Bool,
                  val retires: Vec[Bool]) extends RingBuffer(size, readyFn) {
  require(retires.length == size)
  val retired = RegInit(Vec(size, Bool()), 0.U.asTypeOf(Vec(size, Bool())))

  val set = new {
    def valid(n: UInt) = setValidFn(n)

    def entry(n: UInt) = {
      valid(n)
      setEntryFn(n)
    }
  }

  val clear = new {
    def valid(n: UInt) = clrValidFn(n)

    def entry(n: UInt) = clrEntryFn(n)
  }

  def retire(n: UInt) = {
    clear.valid(n)
    clear.entry(n)
    retireFn(n)
  }


  override def body = {
    val headp1 = step(head, 1)

    when(retires(head) || retired(head)) {
      retired(head) := false.B
      setHead(headp1)
    }

    (0 until size).foreach(i => {
      when(retires(i)) {
        retire(i.U)
        when (i.U =/= head) {
          retired(i.U) := true.B
        }
      }
    })
  }
}

abstract class IssueLogic(val maxStep: Int, val window: IssueWindow, fire: => Bool) {
  require(maxStep <= window.size)
  val t = Seq.tabulate(maxStep)(i => window.step(window.tail, i))

  private val issueItems = mutable.ArrayBuffer[issue]()

  final protected case class issue(name: String, action:Option[() =>Unit] = None, valid:Option[Bool]=None) {
    def apply(action: => Unit): issue = {
      issue(name, Some(()=>action))
    }

    def at(valid: Bool): Unit = {
      issueItems += issue(name, action, Some(valid))
    }
  }

  protected def start(n: UInt) = window.set.entry(n)

  protected def stop(n: UInt) = window.setTail(n)


  final protected def allIssues = issueItems.map { i => {
    require(i.valid.isDefined, s"issueItem ${i.name} lost valid condition!")
    require(i.action.isDefined, s"issueItem ${i.name} lost action!")
    i
  }
  }.map(i => i.valid.get -> i.action.get)

  def logic = {
    when(fire) {
      allIssues.foreach {
        case (valid, action) =>
          when(valid) {
            action()
          }
      }
    }
  }
}
