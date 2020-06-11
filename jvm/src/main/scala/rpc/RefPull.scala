package rpc

import scala.annotation.tailrec
import scala.io.StdIn

object RefPull extends App {
  trait Action
  case class Typed(line: String) extends Action
  type Time = Long

  case class Behavior[A](unB: (List[Action], Time) => A, before: () => A)
  case class Event[A](unE: (List[Action], Time) => Option[A])

  def fix[A](f: (() => A) => A): A = {
    var cache: Option[A] = None
    cache match {
      case None =>
        val b = f(() => cache.get)
        cache = Some(b)
        b
      case Some(b) => b
    }
  }

  object Event {
    def map[A, B](e: Event[A])(f: A => B): Event[B] =
      Event((actions, time) => e.unE(actions, time).map(f))

    def filter[A](e: Event[A])(f: A => Boolean): Event[A] =
      Event((actions, time) => e.unE(actions, time).filter(f))

    def union[A](e1: Event[A], e2: Event[A])(both: (A, A) => A): Event[A] =
      Event { (actions, time) =>
        (e1.unE(actions, time), e2.unE(actions, time)) match {
          case (Some(e1v), Some(e2v)) => Some(both(e1v, e2v))
          case (Some(e1v), None)      => Some(e1v)
          case (None, Some(e2v))      => Some(e2v)
          case (None, None)           => None
        }
      }

    def hold[A](e: Event[A], start: A): Behavior[A] = {
      var x = start
      Behavior({ (actions, time) =>
        e.unE(actions, time) match {
          case Some(a) => x = a; a
          case None    => x
        }
      }, () => x)
    }

    def accumFRecursive[A](a: A, e: Event[A => A]): Behavior[A] = {
      fix { b: (() => Behavior[A]) =>
        Event.hold(Event.map(Behavior.snapshot(Behavior.delayed(b), e)) {
          case (a, f) => f(a)
        }, a)
      }
    }

    // Not a primitive with hold + delayed + fix
    def accumF[A](a: A, e: Event[A => A]): Behavior[A] = {
      var x = a
      Behavior(
        { (actions, time) =>
          e.unE(actions, time) match {
            case Some(f) =>
              val old = x
              x = f(old)
              x
            case None => a
          }
        },
        () => x
      )
    }

    def accum[A, Acc](e: Event[A], start: Acc)(f: (Acc, A) => Acc) = {
      val eF = Event.map(e) { a => (acc: Acc) =>
        f(acc, a)
      }
      accumFRecursive(start, eF)
    }

    def actions: Event[List[Action]] = Event { (actions, _) =>
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
    def pure[A](a: A): Behavior[A] = Behavior((_, _) => a, () => a)
    def app[A, B](f: Behavior[A => B], p: Behavior[A]) =
      Behavior({ (a, t) =>
        f.unB(a, t)(p.unB(a, t))
      }, () => f.before()(p.before()))
    def map[A, B](b: Behavior[A])(f: A => B) = app(pure(f), b)

    // Is only well-defined for all behaviors created from hold or its derivatives; not for primitives
    def delayed[A](bF: () => Behavior[A]): Behavior[A] =
      Behavior((_, _) => bF().before(), () => bF().before())

    def snapshot[A, B](b: Behavior[A], e: Event[B]): Event[(A, B)] =
      Event { (actions, time) =>
        e.unE(actions, time) match {
          case Some(evValue) => Some((b.unB(actions, time), evValue))
          case None          => None
        }
      }

    // time is not a correct implementation
    //   -> what is the value of Time.delayed?
    val time = Behavior((_, t) => t, () => ???)
  }

  object Program {
    def timedLines = Behavior.snapshot(Behavior.time, Event.lines)

    def countLines = Event.accum(Event.lines, 0) { (acc, _) =>
      acc + 1
    }

    def result: Event[String] =
      Event.map(Behavior.snapshot(countLines, timedLines)) {
        case (count, (time, line)) =>
          s"Typed $count:$line @ $time"
      }

    def testHold: Behavior[String] = Event.hold(Event.lines, "START")

    def diamondShape = {
      val lt: Behavior[Int => Int => Boolean] = Behavior.pure(x => y => x > y)
      val larger                              = Behavior.map(countLines)(_ + 1)
      Behavior.app(Behavior.app(lt, larger), countLines)
    }
  }

  val program: (List[Action], Time) => Option[String] = Program.result.unE

  @tailrec
  def run[R](f: (List[Action], Time) => R)(quit: R => Boolean): Unit = {
    println("Starting to read...")
    val result = f(List(Typed(StdIn.readLine())), System.currentTimeMillis())
    if (quit(result)) System.exit(1)
    println(result)
    run(f)(quit)
  }

//  run(Program.result.unE) { case Some(str: String) => str.contains("quit") }
//  run(Program.diamondShape.unB)(_ == false) // runs forever!

  val lines = Program.testHold.unB

  "asd".foreach { char =>
    val result = lines(List(Typed(char.toString)), System.currentTimeMillis())
    println(result)
  }

  println(lines(List.empty, System.currentTimeMillis()))
}
