package rpc

import scala.io.StdIn

trait FAL extends App {

  trait Action
  case class Typed(line: String) extends Action

  type Time = Long

  case class Behavior[A](
      impl: (LazyList[(Option[Action], Time)]) => LazyList[A])

  object Behavior {
    def const[A](a: A): Behavior[A] =
      Behavior((_) => LazyList.continually(a))

    def ap[A, B](bf: Behavior[A => B], bp: => Behavior[A]): Behavior[B] = {
      Behavior { (input) =>
        bp.impl(input).zip(bf.impl(input)).map {
          case (a, f) =>
            f(a)
        }
      }
    }

    def map[A, B](b: Behavior[A])(f: A => B): Behavior[B] =
      Behavior.ap(const(f), b)

    def snapshot[A, B](b: Behavior[A], e: Event[B]): Event[(A, B)] =
      Event(Behavior { (input) =>
        lazy val x: LazyList[Option[B]] = e.b.impl(input)
        lazy val y: LazyList[A]         = b.impl(input)
        x.zip(y).map {
          case (opt, a) =>
            opt.map(a -> _)
        }
      })

    // switch implementation
//    def switch[A](b: => Behavior[A],
//                  switcher: => Event[Behavior[A]]): Behavior[A] = {
//      Behavior { (input) =>
//        def loop(input: LazyList[(Option[Action], Time)],
//                 events: => LazyList[Option[Behavior[A]]],
//                 behaviors: => LazyList[A]): LazyList[A] =
//          behaviors.head #:: loop(input.tail, events.tail, events.head match {
//            case Some(Behavior(sImpl)) =>
//              sImpl(input.tail)
//            case None => behaviors.tail
//          })
//
//        val bOutput      = b.impl(input)
//        lazy val eOutput = switcher.b.impl(input)
//        loop(input, eOutput, bOutput)
//      }
//    }

    def time: Behavior[Time] = Behavior { input =>
      input.map(_._2)
    }
  }

  case class Event[A](b: Behavior[Option[A]])

  object Event {
    def map[A, B](e: Event[A])(f: A => B) =
      Event(Behavior.map(e.b) { optA =>
        optA.map(f)
      })

    def union[A](e1: Event[A], e2: Event[A])(both: (A, A) => A): Event[A] =
      Event(Behavior { input =>
        e1.b.impl(input).zip(e2.b.impl(input)).map {
          case (Some(e1v), Some(e2v)) => Some(both(e1v, e2v))
          case (Some(e1v), None)      => Some(e1v)
          case (None, Some(e2v))      => Some(e2v)
          case (None, None)           => None
        }
      })

    def filter[A](e: Event[A])(f: A => Boolean) =
      Event(Behavior { (input) =>
        e.b.impl(input).filter {
          case None    => false
          case Some(a) => f(a)
        }
      })

    def accum[A, Acc](e: Event[A], start: Acc)(f: (Acc, A) => Acc) = {
      Behavior { input =>
        e.b.impl(input).scanLeft(start) {
          case (acc, Some(a)) => f(acc, a)
          case (acc, None)    => acc
        }
      }
    }

    def actions: Event[Action] = Event {
      Behavior { (input) =>
        input.map(_._1)
      }
    }

    def lines: Event[String] = {
      val typeds = filter(actions) {
        case Typed(_) => true
        case _        => false
      }
      map(typeds) {
        case Typed(str) => str
      }
    }
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
  }

  val timeSource = LazyList.continually {
    println("Getting system time...")
    System.currentTimeMillis()
  }

  // This drives everything, blocking call to readLine()
  private val str = LazyList.continually {
    println("Reading...")
    StdIn.readLine()
  }
  val typeds = str.map(Typed).map(Option(_))

  println("Starting to read...")
  val program = Program.result.b.impl
  val outcome = program(typeds.zip(timeSource))

  for { result <- outcome } {
    result.foreach { str =>
      if (str.contains("quit")) System.exit(1)
      println(str)
    }
  }
}
