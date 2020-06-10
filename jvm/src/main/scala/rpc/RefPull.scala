package rpc

import scala.annotation.tailrec
import scala.io.StdIn

object RefPull extends App {
  trait Action
  case class Typed(line: String) extends Action

  case class Behavior[A](unB: List[Action] => A, before: () => A)
  case class Event[A](unE: List[Action] => Option[A])

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
      Event(e.unE.andThen(_.map(f)))

    def filter[A](e: Event[A])(f: A => Boolean) =
      Event(e.unE.andThen(_.filter(f)))

    def hold[A](e: Event[A], start: A): Behavior[A] = {
      var x = start
      Behavior({ actions =>
        e.unE(actions) match {
          case Some(a) => x = a; a
          case None    => x
        }
      }, () => x)
    }

    def accumFRecursive[A](a: A, e: Event[A => A]): Behavior[A] = {
      fix[Behavior[A]] { b: (() => Behavior[A]) =>
        Event.hold(Event.map(Behavior.snapshot(Behavior.delayed(b), e)) {
          case (a, f) => f(a)
        }, a)
      }
    }

    // Not a primitive with delayed + fix
    def accumF[A](a: A, e: Event[A => A]): Behavior[A] = {
      var x = a
      Behavior(
        { actions =>
          e.unE(actions) match {
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

    def actions: Event[List[Action]] = Event { (actions) =>
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
    def pure[A](a: A): Behavior[A] = Behavior(_ => a, () => a)
    def map[A, B](b: Behavior[A])(f: A => B): Behavior[B] =
      Behavior({ (a) =>
        f(b.unB(a))
      }, () => f(b.before()))
    def app[A, B](f: Behavior[A => B], p: Behavior[A]) =
      Behavior({ a =>
        f.unB(a)(p.unB(a))
      }, () => f.before()(p.before()))

    def delayed[A](bF: () => Behavior[A]): Behavior[A] =
      Behavior(_ => bF().before(), () => bF().before())

    def snapshot[A, B](b: Behavior[A], e: Event[B]): Event[(A, B)] =
      Event { actions =>
        e.unE(actions) match {
          case Some(evValue) => Some((b.unB(actions), evValue))
          case None          => None
        }
      }

    // time is not a correct implementation
    //   -> No memoization per logical time slot
    //   -> This gives different values within the same logical time slot
    //   -> Overall fix is to memoize all sources within a logical time slot
    val time = Behavior(_ => System.currentTimeMillis(),
                        () => System.currentTimeMillis())
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

    def diamondShape = {
      val lt: Behavior[Int => Int => Boolean] = Behavior.pure(x => y => x > y)
      val larger                              = Behavior.map(countLines)(_ + 1)
      Behavior.app(Behavior.app(lt, larger), countLines)
    }
  }

  val program: List[Action] => Option[String] = Program.result.unE

  @tailrec
  def run[R](f: List[Action] => R)(quit: R => Boolean): Unit = {
    println("Starting to read...")
    val result = f(List(Typed(StdIn.readLine())))
    if (quit(result)) System.exit(1)
    println(result)
    run(f)(quit)
  }

  run(Program.result.unE) { case Some(str: String) => str.contains("quit") }

//  run(Program.diamondShape.unB)(_ == false)

//  val lines = Program.countLines.unB
//
//  "as".foreach { char =>
//    val result = lines(List(Typed(char.toString)))
//    println(result)
//  }
}
