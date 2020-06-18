data Action = Typed String;
data Time = {l}. T Int;

data List = [a]. Nil | Cons a (List [a]) ;
data Option = [a]. None | Some a ;

data Behavior = {l}. [a]. Behavior ((Option [Action], Time {l}) -l-> a);
data Event = {l}. [a]. Event ((Option [Action], Time {l}) -l-> Option [a]);

// FIXME: Needs memo1 on all Behavior / Event creations to make sure there are
//    no duplicate computations (important because of side effects!)
//memo1: {l}. [a b]. (a -l-> b) -l-> a -l-> b
//= {l}. [a b]. \f: (a -l-> b) @ l a: a @ l.
//    case (ref {l} [Option [(a, b)]] start, 0) {
//      (cache, i) =>
//        (cache := {client} [Option [(a, b)]] (Some [(a, b)]
//    }

//def memo1[A, B](f: A => B): A => B = {
//  var cache: Option[(A, B)] = None
//  def miss(a: A) = {
//    val newB = f(a)
//    cache = Some(a, newB)
//    newB
//  }
//  (a: A) =>
//    cache match {
//      case Some((cA, cB)) => if (a == cA) cB else miss(a)
//      case None           => miss(a)
//    }
//}

toInt: {l}. Time {l} -l-> Int
= {l}. \t: Time {l} @ l. case t { T i => i};

on: {l}. [a]. (Unit -l-> a) -l-> a
= {l}. [a]. \f: Unit -l-> a @ l. f ();

eImpl : {l}. [a]. Event {l} [a] -l-> ((Option [Action], Time {l}) -l-> Option [a])
= {l}. [a]. \ev: Event {l} [a] @ l. case ev { Event impl => impl };

eMap : {l}. [a b]. (a -l-> b) -l-> Event {l} [a] -l-> Event {l} [b]
= {l}. [a b]. \f: (a -l-> b) @ l ev: Event {l} [a] @ l.
    Event {l} [b] (\in: (Option [Action], Time {l}) @ l.
      case (eImpl {l} [a] ev) in {
        None => None [b];
        Some val => Some [b] (f val)
      });

eFilter : {l}. [a]. (a -l-> Bool) -l-> Event {l} [a] -l-> Event {l} [a]
= {l}. [a]. \f: (a -l-> Bool) @ l ev: Event {l} [a] @ l.
    Event {l} [a] (\in: (Option [Action], Time {l}) @ l.
      case (eImpl {l} [a] ev) in {
        None => None [a];
        Some a => if f a then Some [a] a else None [a]
      });

eUnion : {l}. [a]. (a -l-> a -l-> a) -l-> Event {l} [a] -l-> Event {l} [a] -l-> Event {l} [a]
= {l}. [a]. \f: (a -l-> a -l-> a) @ l ev1: Event {l} [a] @ l ev2: Event {l} [a] @ l.
    Event {l} [a] (\in: (Option [Action], Time {l}) @ l.
      case (eImpl {l} [a] ev1) in {
        None => case (eImpl {l} [a] ev2) in {
          None => None [a];
          Some a2 => Some [a] a2
        };
        Some a1 => case (eImpl {l} [a] ev2) in {
          None => Some [a] a1;
          Some a2 => Some [a] (f a1 a2)
        }
      });

eAccum : {l}. [a b]. (b -l-> (b -l-> a -l-> b) -l-> Event {l} [a] -l-> Event {l} [b])
= {l}. [a b]. \start: b @ l f: (b -l-> a -l-> b) @ l ev: Event {l} [a] @ l.
    case (ref {l} [b] start, 0) {
      (r, i) => Event {l} [b] (\in: (Option [Action], Time {l}) @ l.
        case (eImpl {l} [a] ev) in {
          None => None [b];
          Some a => case (r := {l} [b] (f (! {l} [b] r) a), 0) {
            (i, j) => Some [b] (f (! {l} [b] r) a)
          }
        }
      )
    };

bAccum : {l}. [a b]. (b -l-> (b -l-> a -l-> b) -l-> Event {l} [a] -l-> Behavior {l} [b])
= {l}. [a b]. \start: b @ l f: (b -l-> a -l-> b) @ l ev: Event {l} [a] @ l.
    case (ref {l} [b] start, 0) {
      (r, i) => Behavior {l} [b] (\in: (Option [Action], Time {l}) @ l.
        case (eImpl {l} [a] ev) in {
          None => ! {l} [b] r;
          Some a => case (r := {l} [b] (f (! {l} [b] r) a), 0) {
            (i, j) => ! {l} [b] r
          }
        }
      )
    };

actions : {l}. Event {l} [Action]
= {l}. Event {l} [Action] (\in: (Option [Action], Time {l}) @ l.
  case in { (actionOpt, t) => actionOpt }
);

lines : {l}. Event {l} [String]
= {l}. Event {l} [String] (\in: (Option [Action], Time {l}) @ l.
  case in { (actionOpt, t) => case actionOpt {
    None => None [String];
    Some action => case action {
      Typed str => Some [String] str
    }
  }}
);

// Behavior implementation
bImpl : {l}. [a]. Behavior {l} [a] -l-> ((Option [Action], Time {l}) -l-> a)
= {l}. [a]. \ev: Behavior {l} [a] @ l. case ev { Behavior impl => impl };

bPure : {l}. [a]. a -l-> Behavior {l} [a]
= {l}. [a]. \a: a @ l. Behavior {l} [a] (\in: (Option [Action], Time {l}) @ l. a);

bApp : {l}. [a b]. Behavior {l} [a -l-> b] -l-> Behavior {l} [a] -l-> Behavior {l} [b]
= {l}. [a b]. \fb: Behavior {l} [a -l-> b] @ l pb: Behavior {l} [a] @ l.
    Behavior {l} [b] (\in: (Option [Action], Time {l}) @ l.
      ((bImpl {l} [a -l-> b] fb) in) ((bImpl {l} [a] pb) in)
    );

bSnap : {l}. [a b]. Event {l} [a] -l-> Behavior {l} [b] -l-> Event {l} [(a, b)]
= {l}. [a b]. \ae: Event {l} [a] @ l bb: Behavior {l} [b] @ l.
    Event {l} [(a, b)] (\in: (Option [Action], Time {l}) @ l.
        case (eImpl {l} [a] ae) in {
          None => None [(a, b)];
          Some a => Some [(a, b)] ((a, ((bImpl {l} [b] bb) in)))
        }
    );

// cross-tier implementation

st: Ref {server} [Int] = ref {server} [Int] 0;
incSt: Unit -server-> Int
= \unit: Unit @ server. case (st := {server} [Int] (! {server} [Int] st) + 1, 0) {
  (i, j) => ! {server} [Int] st
};
toServerTime: Time {client} -server-> Time {server}
= \tc: Time {client} @ server. T {server} (incSt ());
toServerInput: (Option [Action], Time {client}) -server-> (Option [Action], Time {server})
= \inC: (Option [Action], Time {client}) @ server. case inC {
    (ac, tc) => (ac, toServerTime tc)
};

ct: Ref {client} [Int] = ref {client} [Int] 0;
incCt: Unit -client-> Int
= \unit: Unit @ client. case (ct := {client} [Int] (! {client} [Int] ct) + 1, 0) {
  (i, j) => ! {client} [Int] ct
};
toClientTime: Time {server} -client-> Time {client}
= \tc: Time {server} @ client. T {client} (incCt ());
toClientInput: (Option [Action], Time {server}) -client-> (Option [Action], Time {client})
= \in: (Option [Action], Time {server}) @ client. case in {
    (ac, t) => (ac, toClientTime t)
};

sTime : Behavior {server} [Time {server}]
= Behavior {server} [Time {server}] (\in: (Option [Action], Time {server}) @ server.
  case in { (actionOpt, t) => T {server} (! {server} [Int] st) }
);
cTime : Behavior {client} [Time {client}]
= Behavior {client} [Time {client}] (\in: (Option [Action], Time {client}) @ client.
  case in { (actionOpt, t) => T {client} (! {client} [Int] ct) }
);

// Can't write this without pattern matching on locations
//time : {l}. Behavior {l} [Time {l}]
//= {l}. Behavior {l} [Time {l}] (\in: (Option [Action], Time {l}) @ l.
//  case in { (actionOpt, t) => t }
//);

eServer: [a]. Event {client} [a] -server-> Event {server} [a]
= [a]. \evc: Event {client} [a] @ server.
    Event {server} [a] (\in: (Option [Action], Time {server}) @ server.
      (eImpl {client} [a] evc) (toClientInput in));

eClient: [a]. Event {server} [a] -client-> Event {client} [a]
= [a]. \evc: Event {server} [a] @ client.
    Event {client} [a] (\in: (Option [Action], Time {client}) @ client.
      (eImpl {server} [a] evc) (toServerInput in));

bServer: [a]. Behavior {client} [a] -server-> Behavior {server} [a]
= [a]. \bc: Behavior {client} [a] @ server.
    Behavior {server} [a] (\in: (Option [Action], Time {server}) @ server.
      (bImpl {client} [a] bc) (toClientInput in));

bClient: [a]. Behavior {server} [a] -client-> Behavior {client} [a]
= [a]. \b: Behavior {server} [a] @ client.
    Behavior {client} [a] (\in: (Option [Action], Time {client}) @ client.
      (bImpl {server} [a] b) (toServerInput in));

// 1st timed lines

timedLines : Event {client} [String]
= eMap {client} [(String, Time {client}) String]
    (\tup: (String, Time {client}) @ client. case tup {
      (line, time) => concat {client} (intToString {client} (toInt {client} time)) line
    })
    (bSnap {client} [String (Time {client})] (lines {client}) cTime);

// server counter -> fold + print

sTimedLines: Event {server} [String]
= eServer [String] timedLines;

serverCount : Behavior {server} [Int]
= bAccum {server} [String Int]
    0
    (\acc: Int @ server new: String @ server.
      case (print {server} new, acc + 1) { (i, c) => c })
    sTimedLines;

// 2nd timed lines -> combine time + server count + timedLines

timedCountTimedLines : Event {client} [String]
= eMap {client} [((String, Int), Time {client}) String]
    (\tup1: ((String, Int), Time {client}) @ client. case tup1 {
      (tup2, time) => case tup2 {
        (line, count) =>
          concat {client}
            (concat {client}
              (intToString {client} (toInt {client} time))
              (intToString {client} count))
            line
      }
    })
    (bSnap {client} [(String, Int) (Time {client})]
      (bSnap {client} [String Int] timedLines (bClient [Int] serverCount))
      cTime);

loop : Unit -client-> Unit
= \u: Unit @ client.
    case (Some [Action] (Typed (read {client} ())), incCt ()) {
      (act, t) => case (eImpl {client} [String] timedCountTimedLines) ((act, T {client} t)) {
        Some result => case (0, print {client} result) {
          (x, y) => loop ()
        }
      }
    };

main : Unit = loop ()


// program

//  def timedLines = Behavior.snapshot(Behavior.time, Event.lines)
//
//  def countLines = Event.accum(Event.lines, 0) { (acc, _) =>
//    acc + 1
//  }
//
//  def result: Event[String] =
//    Event.map(Behavior.snapshot(countLines, timedLines)) {
//      case (count, (time, line)) =>
//        s"Typed $count:$line @ $time"
//    }

