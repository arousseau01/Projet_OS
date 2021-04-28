module type S = sig
  type 'a process
  type 'a in_port
  type 'a out_port
  val new_channel: unit -> 'a in_port * 'a out_port
  val put: 'a -> 'a out_port -> unit process
  val get: 'a in_port -> 'a process
  val doco: unit process list -> unit process
  val return: 'a -> 'a process
  val bind: 'a process -> ('a -> 'b process) -> 'b process
  val run: 'a process -> 'a
end

module VS : S  =
  struct
    let process_list : (unit -> unit) Queue.t = Queue.create ()

    let suivant () =
	    if Queue.is_empty process_list
	    then ()
	    else Queue.pop process_list ()
    let add c = Queue.push c process_list

    type 'a process = ('a -> unit) -> unit
    type 'a in_port = 'a Queue.t
    type 'a out_port = 'a Queue.t

    let new_channel () =
      let q = Queue.create () in (q, q)

    let put (a : 'a) (q : 'a out_port) (c : unit -> unit) =
      add (fun () -> Queue.push a q; c ()) ;
      suivant ()

    let rec get (q : 'a in_port) (c : 'a -> unit) =
	    add
	      (fun () ->
	        if Queue.is_empty q
	        then get q c
	        else c (Queue.pop q)) ;
      suivant ()

    let rec doco (l : unit process list) (c : unit -> unit) = match l with
      | [] -> suivant (); c ()
      | p::q -> add (fun () -> p suivant); doco q c

    let return a = (fun f -> f a)
    let bind (p1 : 'a process) (fp2 : 'a -> 'b process) (c : 'b -> unit) = p1 (fun a -> (fp2 a) c)

    let run p = 
      let result = ref(None) in 
      let c = (fun a -> result:= Some a; suivant ()) in p c;
      match !result with 
        | None -> assert false
        | Some a -> a 

 end

module Lib (K : S) = struct
  let ( >>= ) x f = K.bind x f
  let delay f x =
    (K.return ()) >>= (fun () -> K.return (f x))
end

module Example (K : S) = struct
  
  module Lib = Lib(K)
  open Lib
     
  let integers (qo : int K.out_port) : unit K.process = 
    let rec loop n =
      (K.put n qo) >>= (fun () -> loop (n + 1))
    in
    loop 2
    
  let output (qi : int K.in_port) : unit K.process =
    let rec loop () =
      (K.get qi) >>= (fun v -> Printf.printf "%d\n" v; loop ())
    in
    loop ()
    
  let main : unit K.process =
    (delay K.new_channel ()) >>=
      (fun (q_in, q_out) -> K.doco [ integers q_out ; output q_in ; ])
end

module Test = Example(VS);;

VS.run Test.main;;