data List = [a]. Nil | Cons a (List [a]) ;
data Option = [a]. None | Something a ;

head : {l1}. [a]. (List [a] -l1-> Option [a])
     = {l1}. [a].
     \ls:List [a] @ l1.
          case ls {
            Nil => None [a];
            Cons x xs => Something [a] x
          };

main : Option [Int]
     = head {client} [Int] (Cons [Int] 22 (Nil [Int]))
