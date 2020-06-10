package rpc

object SimpleFirstOrder extends App {
  trait Action
  case class Typed(line: String) extends Action

  case class Event[A](unE: LazyList[Option[A]])
  case class Behavior[A](unB: LazyList[A])

  object Event {
    def map[A, B](e: Event[A])(f: A => B): Event[B] =
      Event(e.unE.map {
        case None    => None
        case Some(a) => Some(f(a))
      })

    def accum[A](a: A, e: Event[A => A]): Behavior[A] = {
      Behavior(
        e.unE.scanLeft(a) {
          case (acc, Some(f)) => f(acc)
          case (acc, None)    => acc
        }
      )
    }
  }

  object Behavior {
    def pure[A](a: A): Behavior[A] = Behavior(LazyList.continually(a))
    def map[A, B](b: Behavior[A])(f: A => B): Behavior[B] =
      Behavior(b.unB.map(f))
    def app[A, B](f: Behavior[A => B], p: Behavior[A]) =
      Behavior(f.unB.lazyZip(p.unB).map(_ apply _))

    def snapshot[A, B](b: Behavior[A], e: Event[B]): Event[(A, B)] = Event(
      b.unB.lazyZip(e.unE).map {
        case (a, Some(b)) => Some((a, b))
        case (a, _)       => None
      }
    )

    val time = Behavior(LazyList.continually(System.currentTimeMillis()))
  }

  object Program {
//    def timedLines = Behavior.snapshot(Behavior.time, Event.lines)
//
//    def countLines = Event.accum(Event.lines, 0) { (acc, _) =>
//      acc + 1
//    }
//
//    def result: Event[String] =
//      Event.map(Behavior.snapshot(countLines, timedLines)) {
//        case (count, (time, line)) =>
//          s"Typed $count:$line @ $time"
//      }
  }
}
