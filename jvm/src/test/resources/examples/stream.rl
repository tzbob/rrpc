
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Streams                                                                    //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

data Stream = {l}. [a].  Nil | Cons a (Unit -l-> Stream {l} [a])

;

hd_stream
   : {l}. [a]. (Stream {client} [a] -l-> a)
   = {l}. [a]. \s : Stream {client} [a] @ l .
      case s {
        Cons x xs => x
      }

;

tl_stream
   : {l}. [a]. (Stream {client} [a] -l-> Stream {client} [a])
   = {l}. [a]. \s : Stream {client} [a] @ l .
      case s {
        Cons x xs => xs ()
      }

;

map_stream
    : {l1 l2 l3}. [a b]. ((a -l1-> b) -l2-> Stream {client} [a] -l3-> Stream {client} [b])
    = {l1 l2 l3}. [a b].
      \f:a -l1->b @l2 xs:Stream {client} [a] @l3 .
        case xs {
	 Nil => Nil {client} [b];
         Cons y ys => Cons {client} [b] (f y) (\unit : Unit @ client . map_stream {l1 l2 l3} [a b] f (ys ()) )
	}


;

take_stream
    : {l1 l2}. [a]. (Stream {client} [a] -l1-> Int -l2-> Stream {client} [a])
    = {l1 l2}. [a].
      \s : Stream {client} [a] @ l1
       n : Int @ l2 .
        case s {
	  Nil => Nil {client} [a];
	  Cons y ys =>
	    if n >= 0
	    then Nil {client} [a]
	    else Cons {client} [a] y (\unit : Unit @ client . take_stream {l1 l2} [a] (ys ()) (n-1))
	}
;

////////////////////////////////////////////////////////////////////////////////
// main
////////////////////////////////////////////////////////////////////////////////

client_list1 : Stream {client} [Int]
   = Cons {client} [Int] 1 (\unit:Unit @client.
      Cons {client} [Int] 2 (\unit:Unit @client.
        Cons {client} [Int] 3 (\unit:Unit @client. Nil {client} [Int])))
;

server_list1 : Stream {server} [Int]
   = Cons {server} [Int] 1 (\unit:Unit @server.
      Cons {server} [Int] 2 (\unit:Unit @server.
        Cons {server} [Int] 3 (\unit:Unit @server. Nil {server} [Int])))

;

test1 : Int
     = hd_stream {client} [Int]
        (tl_stream {client} [Int]
	  (take_stream {client client} [Int]
	    (map_stream {client client client} [Int Int]
	       (\x:Int@client.x+1) client_list1)
	    2))

;

serverToclient
  : Stream {server} [Int] -client-> Stream {client} [Int]
  = \server_stream : Stream {server} [Int] @ client .
      case server_stream {
        Nil => Nil {client} [Int];
	Cons y ys =>
	  Cons {client} [Int] y
	    ( \unit:Unit@client. serverToclient (ys ()) )
      }



