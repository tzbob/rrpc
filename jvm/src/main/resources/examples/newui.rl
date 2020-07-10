data List = [a]. Nil | Cons a (List [a]) ;

data Html = Element String (List [Attr]) (List [Html]) | Txt String;
data Attr = Property String String
          | Attribute String String
          // Event bindings
          // Ignores event and produces Html
          | EventBind String Html
          // Reads value from event target as string
          | ValueBind String (String -client-> Html)
          // Key up events for keycode Int
          | KeyBind Int Html;

onClick: (Unit -client-> Html) -client-> Attr
= \f: Unit -client-> Html @ client. ValueBind "click" (\s: String @ client. f ());

onInput: (String -client-> Html) -client-> Attr
= \htmlF: (String -client-> Html) @ client. ValueBind "input" htmlF;

nlH : List [Html] = Nil [Html];
nlA : List [Attr] = Nil [Attr];
csH : Html -client-> List [Html] -client-> List [Html] = Cons [Html];
csA : Attr -client-> List [Attr] -client-> List [Attr] = Cons [Attr];

counter: Int -client-> Html
= \count: Int @ client.
    Element "div" nlA
      (csH (Txt (intToString {client} count))
        (csH (Element "button" (csA
                (onClick (\u: Unit @ client. counter (count + 1))) nlA) (csH (Txt "Increment") nlH))
          (csH (Element "button" (csA
                (onClick (\u: Unit @ client. counter (count - 1))) nlA) (csH (Txt "Decrement") nlH))
          nlH)));

main : Html = counter 0
