data List a = { Nil | Cons a (List <a>) } ;

count : {l1}. [a]. (List<a> -l1-> Int)
    = {l1}. [a].
      \xs: List<a> @ l1.
        case xs {
          Nil => 0;
          Cons y ys => 1 + (count {l1} [a] ys)
      };

main : Int
     = count {client} [Int] (Cons [Int] 1 (Cons [Int] 2 (Cons [Int] 3 (Nil [Int]))))
