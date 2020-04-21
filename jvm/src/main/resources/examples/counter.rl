data List = [a]. Nil | Cons a (List [a]) ;

nl : [a]. List [a]
   = [a]. Nil [a];

cs : [a]. a -client-> List [a] -client-> List [a]
   = [a]. \w: a @ client
           lst: List [a] @ client. Cons [a] w lst;

data Html = [a]. Element String (List [Attr [a]]) (List [Html [a]]) | Txt String;
data Attr = [a]. Property String String | Attribute String String | ValueBind String (String -client-> a);

// TODO: currently: re-use value bind and rely on JavaScript silent failures
onClick : [a]. a -client-> Attr [a]
        = [a]. \msg: a @ client. ValueBind [a] "click" (\str: String @ client. msg);

onInput : [a]. (String -client-> a) -client-> Attr [a]
        = [a]. \msgF: (String -client-> a) @ client. ValueBind [a] "input" msgF;

// Page is: init x view x update x mount point (query selector e.g., #id)
data Page = [a e]. Page a (a -client-> Html [e]) (e -client-> a -client-> a) String;

data Model = Model Int;
data Msg = Increment | Decrement;

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
          case m { Model int =>
            Element [Msg] "div" nlA
              (csH (Element [Msg] "button"
                        (csA (onClick [Msg] Increment) nlA)
                         (csH (Txt [Msg] "+1") nlH))
              (csH (Element [Msg] "button"
                        (csA (onClick [Msg] Decrement) nlA)
                         (csH (Txt [Msg] "-1") nlH))
              (csH (Element [Msg] "div" nlA (csH (Txt [Msg] (intToString {client} int)) nlH))

              nlH)))
          };

update : Msg -client-> Model -client-> Model
       = \msg: Msg @ client model: Model @ client.
            case model {
              Model int =>
                case msg {
                  Increment => Model (int + 1);
                  Decrement => Model (int - 1)
                }
            };

init : Model
     = Model 0;

main : Page [Model Msg]
     = Page [Model Msg] init view update "#body"
