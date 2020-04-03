data Option = [a]. None | Some a ;

getOrElse : {l1}. [a]. (Option [a] -l1-> a -l1-> a)
     = {l1}. [a].
     \opt:Option [a] @ l1 els:a @ l1.
        case opt {
            Some a => a
        };

main : Int
     = getOrElse {client} [Int] (None [Int]) 5
