package rpc

import scala.io.StdIn

object FrpTest extends App {

  trait Action
  case class Typed(line: String) extends Action

  type Time = Long

  case class Behavior[A](
      impl: (LazyList[Option[Action]], LazyList[Time]) => LazyList[A])

  object Behavior {
    def const[A](a: A): Behavior[A] =
      Behavior((_, _) => LazyList.continually(a))

    def ap[A, B](bf: Behavior[A => B], bp: => Behavior[A]): Behavior[B] = {
      Behavior { (actions, time) =>
        bp.impl(actions, time).zip(bf.impl(actions, time)).map {
          case (a, f) =>
            f(a)
        }
      }
    }

    def map[A, B](b: Behavior[A])(f: A => B): Behavior[B] =
      Behavior.ap(const(f), b)

    def snapshot[A, B](b: Behavior[A], e: Event[B]): Event[(A, B)] =
      Event(Behavior { (actions, times) =>
        lazy val x: LazyList[Option[B]] = e.b.impl(actions, times)
        lazy val y: LazyList[A]         = b.impl(actions, times)
        x.zip(y).map {
          case (opt, a) =>
            opt.map(a -> _)
        }
      })

    def switch[A](b: => Behavior[A],
                  switcher: => Event[Behavior[A]]): Behavior[A] = {
      Behavior { (actions, times) =>
        def loop(actions: LazyList[Option[Action]],
                 times: LazyList[Time],
                 events: => LazyList[Option[Behavior[A]]],
                 behaviors: => LazyList[A]): LazyList[A] =
          behaviors.head #:: loop(actions.tail,
                                  times.tail,
                                  events.tail,
                                  events.head match {
                                    case Some(Behavior(sImpl)) =>
                                      sImpl(actions.tail, times.tail)
                                    case None => behaviors.tail
                                  })

        val bOutput = b.impl(actions, times)
        lazy val eOutput = switcher.b.impl(actions, times)
        loop(actions, times, eOutput, bOutput)
      }
    }

    def time: Behavior[Time] = Behavior { (_, times) =>
      times
    }
  }

  case class Event[A](b: Behavior[Option[A]])

  object Event {
    def map[A, B](e: Event[A])(f: A => B) =
      Event(Behavior.map(e.b) { optA =>
        optA.map(f)
      })

    def filter[A](e: Event[A])(f: A => Boolean) =
      Event(Behavior { (actions, times) =>
        e.b.impl(actions, times).filter {
          case None    => false
          case Some(a) => f(a)
        }
      })

    def accum[A, Acc](e: => Event[A], start: Acc)(f: (Acc, A) => Acc) = {
      val init = Behavior.const(start)
      def b: Behavior[Acc] = {
        def accumulator = Event.map(Behavior.snapshot(b, e)) {
          case (acc, a) =>
            Behavior.const(f(acc, a))
        }
        Behavior.switch(init, accumulator)
      }
      b
    }

    def actions: Event[Action] = Event {
      Behavior { (actions, _) =>
        actions
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
  val outcome = program(typeds, timeSource)

  for { result <- outcome } {
    result.foreach { str =>
      if (str.contains("quit")) System.exit(1)
      println(str)
    }
  }
}
