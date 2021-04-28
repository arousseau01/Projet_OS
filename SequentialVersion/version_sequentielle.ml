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

module Monad = struct (* Inutile mais a servi à comprendre l'article *)
  type 'a m = ('a -> unit) -> unit
  let bind f k = fun c -> f (fun a -> k a c)
  let return x = fun c -> c x

  type action_type = 
    | Atom of action_type m
    | Fork of action_type * action_type
    | Stop
  
  type 'a process = ('a -> action_type) -> action_type 
  let action_fun p = p (fun a -> Stop)
  let atom p c = Atom (bind p (fun a -> return (c a)))
  let stop c = Stop
  let par p1 p2 (c:'a -> action_type) = Fork (p1 c,p2 c) 
  let fork p c = Fork (action_fun p,c ())
  let rec round p_list = match p_list with 
    | [] -> return ()
    | a::a_s -> begin match a with 
      | Atom a_m -> bind a_m ( fun a' -> round (List.concat [a_s;[a']]) )
      | Fork (a1,a2) -> round (List.concat [a_s;[a1;a2]])
      | Stop -> round a_s
    end 

  let run_monade p = round [action_fun p]

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
