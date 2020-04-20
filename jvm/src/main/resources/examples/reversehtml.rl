data List = [a]. Nil | Cons a (List [a]) ;

nl : [a]. List [a]
   = [a]. Nil [a];

cs : [a]. a -client-> List [a] -client-> List [a]
   = [a]. \w: a @ client
           lst: List [a] @ client. Cons [a] w lst;

data Html = [a]. Element String (List [Attr [a]]) (List [Html [a]]) | Txt String;
data Attr = [a]. Property String String | Attribute String String | EventBind String (String -client-> a);

onInput : [a]. (String -client-> a) -client-> Attr [a]
        = [a]. \msgF: (String -client-> a) @ client. EventBind [a] "input" msgF;

// Page is: init x view x update x mount point (query selector e.g., #id)
data Page = [a e]. Page a (a -client-> Html [e]) (e -client-> a -client-> a) String;

data Model = Content String String;
data Msg = Update String;

nlH : List [Html [Msg]]
    = Nil [Html [Msg]];

nlA : List [Attr [Msg]]
    = Nil [Attr [Msg]];

csH : Html [Msg] -client-> List [Html [Msg]] -client-> List [Html [Msg]]
    = cs [Html [Msg]];

csA : Attr [Msg] -client-> List [Attr [Msg]] -client-> List [Attr [Msg]]
    = cs [Attr [Msg]];

view : Model -client-> Html [Msg]
     = \m: Model @ client.
          case m { Content str rev =>
            Element [Msg] "div" nlA
              (csH (Element [Msg] "input"
                        (csA (onInput [Msg] (Update))
                            (csA (Attribute [Msg] "type" "text")
                                (csA (Attribute [Msg] "value" str) nlA)))
                         nlH)
                  (csH (Element [Msg] "div" nlA (csH (Txt [Msg] rev) nlH)) nlH))
          };


update : Msg -client-> Model -client-> Model
       = \msg: Msg @ client model: Model @ client.
            case msg { Update str => Content str (concat {server} (concat {server} str " ") str) };

init : Model
     = Content "" "";

main : Page [Model Msg]
     = Page [Model Msg] init view update "#body"
