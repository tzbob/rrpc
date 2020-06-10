package rpc

import cats.Eval

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn

object OldRefPullMemo1 extends App {
  trait Action
  case class Typed(line: String) extends Action

  type Time = Long

  def memo1[A, B](f: A => B): A => B = {
    var cache: Option[(A, B)] = None
    def miss(a: A) = {
      val newB = f(a)
      cache = Some(a, newB)
      newB
    }
    (a: A) =>
      cache match {
        case Some((cA, cB)) => if (a == cA) cB else miss(a)
        case None           => miss(a)
      }
  }

  case class Behavior[A](_unB: Time => List[Action] => A, before: () => A)
  case class Event[A](unE: Time => List[Action] => Option[A])

  object Event {
    def map[A, B](e: Event[A])(f: A => B): Event[B] =
      Event(time => e.unE(time).andThen(_.map(f)))

    def filter[A](e: Event[A])(f: A => Boolean) =
      Event(time => e.unE(time).andThen(_.filter(f)))

    def hold[A](e: Event[A], start: A): Behavior[A] = {
      var x = start
      Behavior(time => { actions =>
        e.unE(time)(actions) match {
          case Some(a) => x = a; a
          case None    => x
        }
      }, { () =>
        println(s"printing for $this")
        x
      })
    }

    def accumFHold[A](a: A, e: Event[A => A]): Behavior[A] = {
      var stateB: Option[Behavior[A]] = None
      def f(): Behavior[A] = stateB match {
        case None =>
          val b =
            Event.hold(Event.map(Behavior.snapshot(Behavior.delayed(f), e)) {
              case (a, f) =>
                println(s"snapshot $stateB for $a")
                f(a)
            }, a)
          stateB = Some(b)
          b
        case Some(b) => b
      }
      f()
    }

    def accum[A, Acc](e: Event[A], start: Acc)(f: (Acc, A) => Acc) = {
      val eF = Event.map(e) { a => (acc: Acc) =>
        f(acc, a)
      }
      accumFHold(start, eF)
    }

    def actions: Event[List[Action]] = Event { time => (actions) =>
      if (actions.isEmpty) None
      else Some(actions)
    }

    def lines: Event[String] = {
      val typeds = filter(actions) {
        case Typed(_) :: Nil => true
        case _               => false
      }
      map(typeds) {
        case Typed(str) :: Nil => str
      }
    }
  }

  object Behavior {
    def pure[A](a: A): Behavior[A] = Behavior(time => _ => a, () => a)
    def map[A, B](b: Behavior[A])(f: A => B): Behavior[B] =
      Behavior(time => { (a) =>
        f(memo1(b._unB)(time)(a))
      }, () => f(b.before()))
    def app[A, B](f: Behavior[A => B], p: Behavior[A]) =
      Behavior(time => { a =>
        memo1(f._unB)(time)(a)(memo1(p._unB)(time)(a))
      }, () => f.before()(p.before()))

    def delayed[A](bF: () => Behavior[A]): Behavior[A] = {
      Behavior(time => { _ =>
        val x = bF().before()
        println(s"calling delayed from before and got $x")
        x
      }, () => ???)
    }

    def snapshot[A, B](b: Behavior[A], e: Event[B]): Event[(A, B)] =
      Event { time => actions =>
        e.unE(time)(actions) match {
          case Some(evValue) =>
            val behValue = memo1(b._unB)(time)(actions)
            println(s"snapshot the behavior value: $behValue")
            Some((behValue, evValue))
          case None => None
        }
      }

    val time = Behavior(time => _ => time, () => System.currentTimeMillis()) // wrong before
  }

  object Program {
    def timedLines = Behavior.snapshot(Behavior.time, Event.lines)

    def countLines = Event.accum(Event.lines, 0) { (acc, _) =>
      acc + 1
    }

    def result: Event[String] =
      Event.map(Behavior.snapshot(countLines, timedLines)) {
        case (count, (time, line)) =>
          println(s"mapped $count $time $line")
          s"Typed $count:$line @ $time"
      }
  }

  val program: Time => List[Action] => Option[String] = Program.result.unE

  @tailrec
  def run(): Unit = {
    println("Starting to read...")
    val result =
      program(System.currentTimeMillis())(List(Typed(StdIn.readLine())))
    result.foreach { str =>
      if (str.contains("quit")) System.exit(1)
      println(str)
    }
    run()
  }

//  run()
  val lines = Program.countLines._unB

  "as".foreach { char =>
    val result = lines(System.currentTimeMillis())(List(Typed(char.toString)))
    println(result)
  }
}
