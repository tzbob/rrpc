data Option a = { None | Some a } ;

getOrElse : {l1}. [a]. (Option<a> -l1-> a -l1-> a)
     = {l1}. [a].
     \opt:Option<a> @ l1 els:a @ l1.
        case opt {
        None => els;
        Some a => a
        };

main : Int
     = getOrElse {client} [Int] (Some [Int] 20) 5
