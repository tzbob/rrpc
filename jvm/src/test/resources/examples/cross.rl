square : {l1}. (Int -l1-> Int)
   = {l1}.
     \i: Int @ l1.
       i * i;

doublesq : {l1}. (Int -l1-> Int)
   = {l1}.
     \i: Int @ l1.
       (square {client} i) * 2;

main : Int
     = doublesq {server} 10
