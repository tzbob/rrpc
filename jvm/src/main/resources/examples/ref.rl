x : Ref {server} [String] = ref {server} [String] "one two three" ;

y : Unit = x := {server} [String] "four five six" ;

main : String = ! {server} [String] x
