data Action = Typed String;
data Time = T Int;

data Behavior = {l}. [a]. Behavior ((Option [Action], Time) -l-> a);
data Event = {l}. [a]. Event ((Option [Action], Time) -l-> Option [a]);

data List = [a]. Nil | Cons a (List [a]) ;
data Option = [a]. None | Some a ;

eImpl : {l}. [a]. Event {l} [a] -l-> ((Option [Action], Time) -l-> Option [a])
      = {l}. [a]. \ev: Event {l} [a] @ l. case ev { Event impl => impl };

eMap : {l}. [a b]. (a -l-> b) -l-> Event {l} [a] -l-> Event {l} [b]
     = {l}. \f: (a -l-> b) @ l ev: Event {l} [a] @ l.
         Event {l} [b] (\in: (Option [Action], Time) @ l.
           case (eImpl {l} [a] ev) in {
             None => None [b];
             Some val => Some [b] (f val)
           });

eFilter : {l}. [a]. (a -l-> Bool) -l-> Event {l} [a] -l-> Event {l} [a]
     = {l}. \f: (a -l-> b) @ l ev: Event {l} [a] @ l.
         Event {l} [a] (\in: (Option [Action], Time) @ l.
           case (eImpl {l} [a] ev) in {
             None => None [a];
             Some a => if f a then Some [a] a else None [a]
           });

eUnion : {l}. [a]. (a -l-> a -l-> a) -l-> Event {l} [a] -l-> Event {l} [a] -l-> Event {l} [a]
     = {l}. \f: (a -l-> a -l-> a) @ l ev1: Event {l} [a] @ l ev1: Event {l} [a] @ l.
         Event {l} [a] (\in: (Option [Action], Time) @ l.
           case (eImpl {l} [a] ev) in {
             None => None [a];
             Some a => Some [a] (f a)
           });

eAccum : {l}. [a b]. b -l-> (b -l-> a -l-> b) -l-> Event {l} [a] -l-> Behavior {l} [b]
      = {l}. \start: b @ l ev: Event {l} [a] @ l.
          let { r: Ref {l} [b] = ref {l} [b] start }
            Behavior {l} [b] (\in: (Option [Action], Time) @ l.
              case (eImpl {l} [a] ev) in {
                None => ! {l} [b] r;
                Some a =>
                  let { u: Unit = r := {l} [b] f (! {l} [b] r) a }
                    ! {l} [b] r
                  end
              }
            )
          end



//  object Event {
//    def map[A, B](e: Event[A])(f: A => B): Event[B] =
//      Event((actions, time) => e.unE(actions, time).map(f))
//
//    def filter[A](e: Event[A])(f: A => Boolean): Event[A] =
//      Event((actions, time) => e.unE(actions, time).filter(f))
//
//    def union[A](e1: Event[A], e2: Event[A])(both: (A, A) => A): Event[A] =
//      Event { (actions, time) =>
//        (e1.unE(actions, time), e2.unE(actions, time)) match {
//          case (Some(e1v), Some(e2v)) => Some(both(e1v, e2v))
//          case (Some(e1v), None)      => Some(e1v)
//          case (None, Some(e2v))      => Some(e2v)
//          case (None, None)           => None
//        }
//      }
//
//    def hold[A](e: Event[A], start: A): Behavior[A] = {
//      var x = start
//      Behavior({ (actions, time) =>
//        e.unE(actions, time) match {
//          case Some(a) => x = a; a
//          case None    => x
//        }
//      }, () => x)
//    }
//
//    def accumFRecursive[A](a: A, e: Event[A => A]): Behavior[A] = {
//      fix { b: (() => Behavior[A]) =>
//        Event.hold(Event.map(Behavior.snapshot(Behavior.delayed(b), e)) {
//          case (a, f) => f(a)
//        }, a)
//      }
//    }
//
//    // Not a primitive with hold + delayed + fix
//    def accumF[A](a: A, e: Event[A => A]): Behavior[A] = {
//      var x = a
//      Behavior(
//        { (actions, time) =>
//          e.unE(actions, time) match {
//            case Some(f) =>
//              val old = x
//              x = f(old)
//              x
//            case None => a
//          }
//        },
//        () => x
//      )
//    }
//
//    def accum[A, Acc](e: Event[A], start: Acc)(f: (Acc, A) => Acc) = {
//      val eF = Event.map(e) { a => (acc: Acc) =>
//        f(acc, a)
//      }
//      accumFRecursive(start, eF)
//    }
//
//    def actions: Event[List[Action]] = Event { (actions, _) =>
//      if (actions.isEmpty) None
//      else Some(actions)
//    }
//
//    def lines: Event[String] = {
//      val typeds = filter(actions) {
//        case Typed(_) :: Nil => true
//        case _               => false
//      }
//      map(typeds) {
//        case Typed(str) :: Nil => str
//      }
//    }
//  }
//
//  object Behavior {
//    def pure[A](a: A): Behavior[A] = Behavior((_, _) => a, () => a)
//    def app[A, B](f: Behavior[A => B], p: Behavior[A]) =
//      Behavior({ (a, t) =>
//        f.unB(a, t)(p.unB(a, t))
//      }, () => f.before()(p.before()))
//    def map[A, B](b: Behavior[A])(f: A => B) = app(pure(f), b)
//
//    // Is only well-defined for all behaviors created from hold or its derivatives; not for primitives
//    def delayed[A](bF: () => Behavior[A]): Behavior[A] =
//      Behavior((_, _) => bF().before(), () => bF().before())
//
//    def snapshot[A, B](b: Behavior[A], e: Event[B]): Event[(A, B)] =
//      Event { (actions, time) =>
//        e.unE(actions, time) match {
//          case Some(evValue) => Some((b.unB(actions, time), evValue))
//          case None          => None
//        }
//      }
//
//    // time is not a correct implementation
//    //   -> what is the value of Time.delayed?
//    val time = Behavior((_, t) => t, () => ???)
