data Action = Typed String;
data Time = {l}. T Int;

data List = [a]. Nil | Cons a (List [a]) ;

any : {l}. List [Bool] -l-> Bool
    = {l}.
      \xs: List [Bool] @ l. case xs {
        Nil => False;
        Cons t xss => if t then t else any {l} xss
      };

merge : {l}. [a]. List [a] -l-> List [a] -l-> List [a]
    = {l}. [a].
      \xs: List [a] @ l ys: List [a] @ l .
        case xs {
         Nil => ys;
         Cons x xss => merge {l} [a] xss (Cons [a] x ys)
        };

map : {l1 l2}. [a b]. (a -l1-> b) -l2-> List [a] -l2-> List [b]
    = {l1 l2}. [a b].
      \f: a -l1-> b @ l2 xs: List [a] @ l2 .
        case xs {
         Nil => Nil [b];
         Cons y ys => Cons [b] (f y) (map {l1 l2} [a b] f ys)
        };

data Option = [a]. None | Some a ;

// action x time x remotes
data In = {l}. In (Option [Action]) (Time {l});

actTime : {l}. In {l} -l-> (Option [Action], Time {l})
= {l}. \in: In {l} @ l. case in { In action time => (action, time) };

// ToServer contains functions that will update all 'ToServer' server values properly.
data ToServer = ToServer
  (In {client} -client-> Bool)
  (Unit -client-> Unit)
;

data ToClient = ToClient
  (In {server} -server-> Bool)
  (Unit -server-> Unit)
;

// Behavior : tos x toc x impl x epoch value
data Behavior = {l}. [a]. Behavior (List [ToServer]) (List [ToClient]) (In {l} -l-> a) a;
behavior: {l}. [a]. List [ToServer] -client-> List [ToClient] -client-> (In {l} -l-> a) -client-> a -client-> Behavior {l} [a]
= {l}. [a]. \tos: List [ToServer] @ client toc: List [ToClient] @ client f: (In {l} -l-> a) @ client epoch: a @ client.
  Behavior {l} [a] tos toc (memo1 {l} {l} [a] f) epoch;
// Event : tos x toc x impl
data Event = {l}. [a]. Event (List [ToServer]) (List [ToClient]) (In {l} -l-> Option [a]);
event: {l}. [a]. List [ToServer] -client-> List [ToClient] -client-> (In {l} -l-> Option [a]) -client-> Event {l} [a]
= {l}. [a]. \tos: List [ToServer] @ client toc: List [ToClient] @ client f: (In {l} -l-> Option [a]) @ client.
  Event {l} [a] tos toc (memo1 {l} {l} [Option [a]] f);

memo1: {l1 l2}. [b]. (In {l1} -l2-> b) -l2-> In {l1} -l2-> b
= {l1 l2}. [b].
  \f: (In {l1} -l2-> b) @ l2 in: In {l1} @ l2.
    let { a: Int = case in { In ac time => case time { T i => i }}}
      let { cache: Ref {l2} [Option [(Int, b)]] = ref {l2} [Option [(Int, b)]] (None [(Int, b)]) }
        let { miss: Int -l2-> b = \missedA: Int @ l2.
          let { b: b = f in }
            let { ignore: Unit = cache := {l2} [Option [(Int, b)]] (Some [(Int, b)] ((a, b))) }
              b
            end
          end
        }
          case ! {l2} [Option [(Int, b)]] cache {
            Some ab => case ab { (ca, cb) => if a >= ca then if a <= ca then cb else miss a else miss a };
            None => miss a
          }
        end
      end
    end;

on: {l}. [a]. (Unit -l-> a) -l-> a
= {l}. [a]. \f: Unit -l-> a @ l. f ();

eImpl : {l}. [a]. Event {l} [a] -l-> (In {l} -l-> Option [a])
= {l}. [a]. \ev: Event {l} [a] @ l. case ev { Event x y impl => impl };

eToS : {l}. [a]. Event {l} [a] -l-> List [ToServer]
= {l}. [a]. \ev: Event {l} [a] @ l. case ev { Event tos x y => tos };

eToC : {l}. [a]. Event {l} [a] -l-> List [ToClient]
= {l}. [a]. \ev: Event {l} [a] @ l. case ev { Event x toc y => toc };

eMap : {l}. [a b]. (a -l-> b) -l-> Event {l} [a] -l-> Event {l} [b]
= {l}. [a b]. \f: (a -l-> b) @ l ev: Event {l} [a] @ l.
    event {l} [b] (eToS {l} [a] ev) (eToC {l} [a] ev) (\in: In {l} @ l.
      case (eImpl {l} [a] ev) in {
        None => None [b];
        Some val => Some [b] (f val)
      });

eFilter : {l}. [a]. (a -l-> Bool) -l-> Event {l} [a] -l-> Event {l} [a]
= {l}. [a]. \f: (a -l-> Bool) @ l ev: Event {l} [a] @ l.
    event {l} [a] (eToS {l} [a] ev) (eToC {l} [a] ev) (\in: In {l} @ l.
      case (eImpl {l} [a] ev) in {
        None => None [a];
        Some a => if f a then Some [a] a else None [a]
      });

eUnion : {l}. [a]. (a -l-> a -l-> a) -l-> Event {l} [a] -l-> Event {l} [a] -l-> Event {l} [a]
= {l}. [a]. \f: (a -l-> a -l-> a) @ l ev1: Event {l} [a] @ l ev2: Event {l} [a] @ l.
    event {l} [a]
      (merge {l} [ToServer] (eToS {l} [a] ev1) (eToS {l} [a] ev2))
      (merge {l} [ToClient] (eToC {l} [a] ev1) (eToC {l} [a] ev2))
      (\in: In {l} @ l.
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
      (r, i) => event {l} [b] (eToS {l} [a] ev) (eToC {l} [a] ev) (\in: In {l} @ l.
        case (eImpl {l} [a] ev) in {
          None => None [b];
          Some a => case (r := {l} [b] (f (! {l} [b] r) a), 0) {
            (i, j) => Some [b] (f (! {l} [b] r) a)
          }
        }
      )
    };

actions : {l}. Event {l} [Action]
= {l}. event {l} [Action] (Nil [ToServer]) (Nil [ToClient]) (\in: In {l} @ l.
  case in { In actionOpt t => actionOpt }
);

lines : {l}. Event {l} [String]
= {l}. event {l} [String] (Nil [ToServer]) (Nil [ToClient]) (\in: In {l} @ l.
  case in { In actionOpt t => case actionOpt {
    None => None [String];
    Some action => case action {
      Typed str => Some [String] str
    }
  }}
);

// Behavior implementation
bImpl : {l}. [a]. Behavior {l} [a] -l-> (In {l} -l-> a)
= {l}. [a]. \ev: Behavior {l} [a] @ l. case ev { Behavior x y impl e => impl };

bToS : {l}. [a]. Behavior {l} [a] -l-> List [ToServer]
= {l}. [a]. \ev: Behavior {l} [a] @ l. case ev { Behavior tos x y e => tos };

bToC : {l}. [a]. Behavior {l} [a] -l-> List [ToClient]
= {l}. [a]. \ev: Behavior {l} [a] @ l. case ev { Behavior x toc y e => toc };

bEpoch : {l}. [a]. Behavior {l} [a] -l-> a
= {l}. [a]. \ev: Behavior {l} [a] @ l. case ev { Behavior x toc y e => e };

bPure : {l}. [a]. a -l-> Behavior {l} [a]
= {l}. [a]. \a: a @ l. Behavior {l} [a] (Nil [ToServer]) (Nil [ToClient]) (\in: In {l} @ l. a) a;

bApp : {l}. [a b]. Behavior {l} [a -l-> b] -l-> Behavior {l} [a] -l-> Behavior {l} [b]
= {l}. [a b]. \fb: Behavior {l} [a -l-> b] @ l pb: Behavior {l} [a] @ l.
    behavior {l} [b]
      (merge {l} [ToServer] (bToS {l} [a -l-> b] fb) (bToS {l} [a] pb))
      (merge {l} [ToClient] (bToC {l} [a -l-> b] fb) (bToC {l} [a] pb))
      (\in: In {l} @ l. ((bImpl {l} [a -l-> b] fb) in) ((bImpl {l} [a] pb) in))
      ((bEpoch {l} [a -l-> b] fb) (bEpoch {l} [a] pb));

bAccum : {l}. [a b]. (b -l-> (b -l-> a -l-> b) -l-> Event {l} [a] -l-> Behavior {l} [b])
= {l}. [a b]. \start: b @ l f: (b -l-> a -l-> b) @ l ev: Event {l} [a] @ l.
    case (ref {l} [b] start, 0) {
      (r, i) => behavior {l} [b]
        (eToS {l} [a] ev)
        (eToC {l} [a] ev)
        (\in: In {l} @ l.
          case (eImpl {l} [a] ev) in {
            None => ! {l} [b] r;
            Some a => case (r := {l} [b] (f (! {l} [b] r) a), 0) {
              (i, j) => ! {l} [b] r
            }
          }
        )
        start
    };

bSnap : {l}. [a b]. Event {l} [a] -l-> Behavior {l} [b] -l-> Event {l} [(a, b)]
= {l}. [a b]. \ae: Event {l} [a] @ l bb: Behavior {l} [b] @ l.
    event {l} [(a, b)]
      (merge {l} [ToServer] (eToS {l} [a] ae) (bToS {l} [b] bb))
      (merge {l} [ToClient] (eToC {l} [a] ae) (bToC {l} [b] bb))
      (\in: In {l} @ l.
        case (eImpl {l} [a] ae) in {
          None => None [(a, b)];
          Some a => Some [(a, b)] ((a, ((bImpl {l} [b] bb) in)))
        }
      );

// cross-tier implementation
toInt: {l}. Time {l} -l-> Int
= {l}. \t: Time {l} @ l. case t { T i => i};

st: Ref {server} [Int] = ref {server} [Int] 0;
incSt: Unit -server-> Int
= \unit: Unit @ server. case (st := {server} [Int] (! {server} [Int] st) + 1, 0) {
  (i, j) => ! {server} [Int] st
};

ct: Ref {client} [Int] = ref {client} [Int] 0;
incCt: Unit -client-> Int
= \unit: Unit @ client. case (ct := {client} [Int] (! {client} [Int] ct) + 1, 0) {
  (i, j) => ! {client} [Int] ct
};

sTime : Behavior {server} [Time {server}]
= behavior {server} [Time {server}]
    (Nil [ToServer])
    (Nil [ToClient])
    (\in: In {server} @ server.
      case in { In actionOpt t => T {server} (! {server} [Int] st) }
    )
    (T {server} 0);
cTime : Behavior {client} [Time {client}]
= behavior {client} [Time {client}]
    (Nil [ToServer])
    (Nil [ToClient])
    (\in: In {client} @ client.
      case in { In actionOpt t => T {client} (! {client} [Int] ct) }
    )
    (T {client} 0);

// Can't write this without pattern matching on locations
//time : {l}. Behavior {l} [Time {l}]
//= {l}. Behavior {l} [Time {l}] (\in: (Option [Action], Time {l}) @ l.
//  case in { (actionOpt, t) => t }
//);

eSetter: {l1 l2}. [a]. Event {l1} [a] -l1-> (Ref {l2} [Option [a]], In {l1} -l1-> Bool, Unit -l1-> Unit)
= {l1 l2}. [a]. \evs: Event {l1} [a] @ l1.
  let { r: Ref {l2} [Option [a]] = ref {l2} [Option [a]] (None [a]) }
    let { set: In {l1} -l1-> Bool = (\in: In {l1} @ l1.
       case (eImpl {l1} [a] evs) in {
         Some aV =>
           let { ignore: Unit = r := {l2} [Option [a]] (Some [a] aV) }
             True
           end;
         None => False
       }
    )}
      let { reset: Unit -l1-> Unit = (\u: Unit @ l1. r := {l2} [Option [a]] (None [a])) }
        (r, set, reset)
      end
    end
  end;

eToServer: [a]. Event {client} [a] -client-> Event {server} [a]
= [a]. \evc: Event {client} [a] @ client. case eSetter {client server} [a] evc {
  (r, set, reset) =>
    event {server} [a]
      (Cons [ToServer] (ToServer set reset) (eToS {client} [a] evc))
      (eToC {client} [a] evc)
      (\in: In {server} @ server. ! {server} [Option [a]] r)
};

eToClient: [a]. Event {server} [a] -client-> Event {client} [a]
= [a]. \evs: Event {server} [a] @ client. case eSetter {server client} [a] evs {
  (r, set, reset) =>
    event {client} [a]
      (eToS {server} [a] evs)
      (Cons [ToClient] (ToClient set reset) (eToC {server} [a] evs))
      (\in: In {client} @ client. ! {client} [Option [a]] r)
};

bSetter: {l1 l2}. [a]. Behavior {l1} [a] -l1-> (Ref {l2} [a], In {l1} -l1-> Bool, Unit -l1-> Unit)
= {l1 l2}. [a]. \b: Behavior {l1} [a] @ l1.
  let { r: Ref {l2} [a] = ref {l2} [a] (bEpoch {l1} [a] b) }
    let { set: In {l1} -l1-> Bool = (\in: In {l1} @ l1.
      let { ignore: Unit = r := {l2} [a] ((bImpl {l1} [a] b) in) }
        True
      end
    )}
      let { reset: Unit -l1-> Unit = (\u: Unit @ l1. ()) }
        (r, set, reset)
      end
    end
  end;

bToServer: [a]. Behavior {client} [a] -client-> Behavior {server} [a]
= [a]. \bc: Behavior {client} [a] @ client. case bSetter {client server} [a] bc {
  (r, set, reset) =>
    behavior {server} [a]
      // behaviors don't reset, they retain the previous value: TODO check if this breaks continuity model
      (Cons [ToServer] (ToServer set reset) (bToS {client} [a] bc))
      (bToC {client} [a] bc)
      (\in: In {server} @ server. ! {server} [a] r)
      (bEpoch {client} [a] bc)
};

bToClient: [a]. Behavior {server} [a] -client-> Behavior {client} [a]
= [a]. \bs: Behavior {server} [a] @ client. case bSetter {server client} [a] bs {
  (r, set, reset) =>
    behavior {client} [a]
      (bToS {server} [a] bs)
      (Cons [ToClient] (ToClient set reset) (bToC {server} [a] bs))
      (\in: In {client} @ client. ! {client} [a] r)
      (bEpoch {server} [a] bs)
};

//
//
//  Program
//
//

// 1st timed lines

timedLines : Event {client} [String]
= eMap {client} [(String, Time {client}) String]
    (\tup: (String, Time {client}) @ client. case tup {
      (line, time) => concat {client} (intToString {client} (toInt {client} time)) line
    })
    (bSnap {client} [String (Time {client})] (lines {client}) cTime);

// server counter -> fold + print

sTimedLines: Event {server} [String]
= eToServer [String] timedLines;

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
      (bSnap {client} [String Int] timedLines (bToClient [Int] serverCount))
      cTime);


//
//
//  Runner
//
//

loop : Event {client} [String] -client-> Unit
= \ev: Event {client} [String] @ client.
    case (Some [Action] (Typed (read {client} ())), incCt ()) {
      // Increase client time & execute the main FRP program
      (act, t) => case (eImpl {client} [String] ev) (In {client} act (T {client} t)) {
        Some result =>
          // Print the result from the first execution
          let { ignore: Unit = print {client} result }
            // Push all ToServers
            let { isSets: List [Bool] =
              map {client client} [ToServer Bool] (\tos: ToServer @ client. case tos {
                ToServer set reset => set ((In {client} act (T {client} t)))
              }) (eToS {client} [String] ev)
            }
              let { ignore: Unit =
                if any {client} isSets then
                  // Increase server time & push all ToClients TODO: fix time, this is a race condition in an asynchronous server!
                  let { ignore: List [Bool] =
                    map {server server} [ToClient Bool] (\toc: ToClient @ server. case toc {
                      ToClient set reset => set ((In {server} act (T {server} (incSt ()))))
                    }) (eToC {client} [String] ev)
                  }
                    // Increase client time & print new results
                    case (eImpl {client} [String] ev) (In {client} (None [Action]) (T {client} (incCt ()))) {
                      Some result => print {client} result;
                      None => ()
                    }
                  end
                else ()
              }
                // reset all
                let { ignore: List [Unit] =
                  map {client client} [ToServer Unit] (\tos: ToServer @ client. case tos {
                    ToServer set reset => reset ()
                  }) (eToS {client} [String] ev)
                }
                  let { ignore: List [Unit] =
                    map {server server} [ToClient Unit] (\toc: ToClient @ server. case toc {
                      ToClient set reset => reset ()
                    }) (eToC {client} [String] ev)
                  }
                    loop ev
                  end
                end
              end
            end
          end;
        None => ()
      }
    };

//
//
// Glitch examples
//
//

x: Behavior {client} [String] =
  bAccum {client} [String String] "" (\acc: String @ client n: String @ client. n) (lines {client});
xx: Behavior {server} [String] = bToServer [String] x;
noGlitch: Behavior {server} [String] =
  bApp {server} [String String] (bApp {server} [String (String -server-> String)]
    (bPure {server} [String -server-> String -server-> String] (\x: String @ server xx: String @ server. concat {server} x xx))
    (bToServer [String] x))
    xx;

y: Behavior {client} [String] =
  bAccum {client} [String String] "" (\acc: String @ client n: String @ client. n) (lines {client});
yy: Behavior {server} [String] = bToServer [String] y;
yyy: Behavior {client} [String] = bToClient [String] yy;
glitch: Behavior {client} [String] =
  bApp {client} [String String] (bApp {client} [String (String -client-> String)]
    (bPure {client} [String -client-> String -client-> String] (\x: String @ client xx: String @ client. concat {client} x xx))
    y)
    yyy;


main : Unit = loop timedCountTimedLines
//main : Unit = 5

