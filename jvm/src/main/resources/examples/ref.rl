x : Ref {server} [String] = ref {server client} [String] "one two three" ;

y : Unit = x := {server client} [String] "four five six" ;

main : String = ! {server client} [String] x
