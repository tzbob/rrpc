package rpc

object FrpDef {
  type Time = Long

  trait EventFunctions[Event[_], Behavior[_]] {
    def map[A, B](e: Event[A])(f: A => B): Event[B]
    def filter[A](e: Event[A])(f: A => Boolean): Event[A]

    def hold[A](e: Event[A], start: A): Behavior[A]
    def accum[A, Acc](e: Event[A], start: Acc)(f: (Acc, A) => Acc): Behavior[A]
  }

  trait BehaviorFunctions[Event[_], Behavior[_]] {
    def pure[A](a: A): Behavior[A]
    def app[A, B](f: Behavior[A => B], p: Behavior[A])
    def snapshot[A, B](b: Behavior[A], e: Event[B]): Event[(A, B)]
  }

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
}
