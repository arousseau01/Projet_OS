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

module Instance (*: (S with type 'a process = 'a -> action_type -> action_type) problème de signature *) = struct 
  type 'a m = ('a -> unit) -> unit
  let bind f k = fun c -> f (fun a -> k a c)
  let return x = fun c -> c x

  type action_type = 
    | Atom of action_type m
    | Fork of action_type*action_type
    | Stop
  
  type 'a process = ('a -> action_type) -> action_type (* ce sont les process réels *)
  let action_fun p = p (fun a -> Stop)
  let atom p c = Atom (bind p (fun a -> return (c a)))
  let stop c = Stop
  let par p1 p2 c = Fork (p1 c,p2 c) 
  let fork p c = Fork (action_fun p,c ())
  let rec round p_list = match p_list with 
    | [] -> return ()
    | a::a_s -> begin match a with 
      | Atom a_m -> bind a_m (fun a' -> round (List.concat [a_s;[a']]))
      | Fork (a1,a2) -> round (List.concat [a_s;[a1;a2]])
      | Stop -> round a_s
    end 

  let run_monade p = round [action_fun p] (* A modifier *)

  (* let run p a = let f = run_monade p in f (fun () -> ()); a *)

  (* A finir (Mutex ?) *)
  type 'a in_port = 'a Queue.t
  type 'a out_port = 'a Queue.t
  let new_channel () = (Queue.create (),Queue.create ())
  let put a q f = atom (fun g -> Queue.add a q; g ()) f
  let get q f = atom (fun g -> g (Queue.pop q)) f 


  let rec doco l f = 
    match l with 
      | [] -> f ()
      | t::q -> fork t (fun () -> doco q f)

end

(*module Instance2 (*: (S with type 'a process = 'a -> action_type -> action_type) problème de signature *) = struct 
  type 'a m = ('a -> unit) -> unit
  let bind_m f k = fun c -> f (fun a -> k a c) (* A changer *)
  let return_m x = fun c -> c x (* A changer *)

  type action_type = 
    | Atom of action_type m
    | Fork of action_type*action_type
    | Stop
  
  type 'a inter_process = { todo : ('a -> action_type * 'a) -> action_type * 'a; value : 'a } (* ce sont les process réels *)
  let action_fun p = p.todo (fun a -> Stop,a)
  let atom p a = { todo = (fun c -> Atom (bind_m p (fun a -> return_m (c a)))) ; value = a } (* Les process doivent avoir une valeur initiale *)
  let stop a = { todo = (fun c -> Stop) ; value = a }
  let par p1 p2 = { todo = (fun c -> Fork (p1.todo c,p2.todo c)) ; value = p1.value } 
  let fork p c = Fork (action_fun p,c ())
  let rec round p_list = match p_list with 
    | [] -> return_m ()
    | a::a_s -> begin match a with 
      | Atom a_m -> bind_m a_m (fun a' -> round (List.concat [a_s;[a']]))
      | Fork (a1,a2) -> round (List.concat [a_s;[a1;a2]])
      | Stop -> round a_s
    end 
  let run_monade p = round [action_fun p] (* A modifier *)

  (* let run p a = let f = run_monade p in f (fun () -> ()); a *)

  let rec doco_inter l f = 
    match l with 
      | [] -> f ()
      | t::q -> fork (fst t) (fun () -> doco_inter q f)

  (* A finir (Mutex ?) *)
  type 'a process = 'a inter_process * 'a
  type 'a in_port = 'a Queue.t
  type 'a out_port = 'a Queue.t
  
  let new_channel () = (Queue.create (),Queue.create ())
  let put a q = (fun f -> atom (fun g -> Queue.add a q; g ()) f), ()
  let get q = let a = Queue.pop q in (fun f -> atom (fun g -> g a) f), a

  let doco l = (fun f -> doco_inter l f), ()

  let return (a:'a) = (fun (c:('a -> action_type)) -> Stop), a
  let bind (p:'a process) (f:'a -> 'b process) = run_monade (fst p)
end*)
