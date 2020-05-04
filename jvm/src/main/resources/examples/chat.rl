thunk : [a]. (Unit -server-> a) -server-> a
      = [a]. \f: Unit -server-> a @ server. f ();

data List = [a]. Nil | Cons a (List [a]) ;

map : {l1 l2 l3}. [a b]. ((a -l1-> b) -l2-> List [a] -l3-> List [b])
    = {l1 l2 l3}. [a b].
      \f:a -l1->b @l2 xs:List [a] @l3 .
        case xs {
         Nil => Nil [b];
         Cons y ys => Cons [b] (f y) (map {l1 l2 l3} [a b] f ys)
        };

nl : [a]. List [a]
   = [a]. Nil [a];

cs : [a]. a -client-> List [a] -client-> List [a]
   = [a]. \w: a @ client
           lst: List [a] @ client. Cons [a] w lst;

data Html = [a]. Element String (List [Attr [a]]) (List [Html [a]]) | Txt String;
data Attr = [a]. Property String String | Attribute String String | EventBind String a | ValueBind String (String -client-> a);

onClick : [a]. a -client-> Attr [a]
        = [a]. \msg: a @ client. ValueBind [a] "click" (\str: String @ client. msg);
onInput : [a]. (String -client-> a) -client-> Attr [a]
        = [a]. \msgF: (String -client-> a) @ client. ValueBind [a] "input" msgF;

// Page is: init x view x update x mount point (query selector e.g., #id)
data Page = [a e]. Page a (a -client-> Html [e]) (e -client-> a -client-> a) String;

data Model = Content String (Ref {server} [List [String]]);
data Msg = Update String | Submit;

nlH : List [Html [Msg]]
    = Nil [Html [Msg]];

nlA : List [Attr [Msg]]
    = Nil [Attr [Msg]];

csH : Html [Msg] -client-> List [Html [Msg]] -client-> List [Html [Msg]]
    = cs [Html [Msg]];

csA : Attr [Msg] -client-> List [Attr [Msg]] -client-> List [Attr [Msg]]
    = cs [Attr [Msg]];

viewChat : List [String] -client-> List [Html [Msg]]
         = \ls: List [String] @ client.
             map {client client client} [String Html[Msg]]
               (\str: String @ client. Element [Msg] "li" nlA (csH (Txt [Msg] str) nlH)) ls;

view : Model -client-> Html [Msg]
     = \m: Model @ client.
          case m { Content str ref =>
            Element [Msg] "div" nlA
              (csH (Element [Msg] "input"
                        (csA (onInput [Msg] Update)
                            (csA (Attribute [Msg] "type" "text")
                                (csA (Attribute [Msg] "value" str) nlA)))
                         nlH)

                  (csH (Element [Msg] "button" (csA (onClick [Msg] Submit) nlA) (csH (Txt [Msg] "Send") nlH))

                      (csH (Element [Msg] "ul" nlA (viewChat (! {server} [List[String]] ref)))

                      nlH)))
          };


update : Msg -client-> Model -client-> Model
       = \msg: Msg @ client model: Model @ client.
          case model { Content line ref =>
            case msg {
              Update str => Content str ref;
              Submit =>
                case (((\x: Unit @ server.
                  ref := {server} [List [String]] (Cons [String] line (! {server} [List [String]] ref))
                ) ()), model) { (u, m) => m }
          }};


serverModel : Ref {server} [List [String]]
            = thunk [Ref {server} [List [String]]] (\u: Unit @ server. ref {server} [List [String]] (Nil [String]));

init : Model
     = Content "" serverModel;

main : Page [Model Msg]
     = Page [Model Msg] init view update "#body"
