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

module Version_Sequentielle_sans_Mutex : (S with type 'a process = ('a -> unit) -> unit) = struct 
  type 'a process = ('a -> unit) -> unit
  type 'a in_port = 'a Queue.t
  type 'a out_port = 'a Queue.t

  let new_channel () :'a in_port * 'a out_port = (Queue.create (),Queue.create ())
  let put (a:'a) (q:'a out_port) (c:unit -> unit) = c (Queue.add a q)
  let get (q:'a in_port) (c:'a -> unit) = c (Queue.pop q) 

  let rec doco (l:unit process list)  (c:unit -> unit) = 
    match l with 
      | [] -> c ()
      | t::q -> t (fun () -> doco q c)

  let return (x:'a) (c:'a -> unit) = c x
  let bind (p1:'a process) (fp2:'a -> 'b process) (c: 'b -> unit) = p1 (fun a -> (fp2 a) c)

  let run (p:'a process) = 
    let result = ref(None) in 
    let c = (fun a -> result:= Some a) in p c;
    match !result with 
      | None -> assert false
      | Some a -> a 

end


module Mutex = struct
  type t = Locked | Unlocked
  let create () = let m = ref Unlocked in m
  let lock (m: t ref) =
    match !m with
    | Unlocked -> ignore(m := Locked)
    | Locked -> failwith "Mutex already locked"
              
  let unlock (m :t ref) =
    match !m with
    | Locked -> ignore(m := Unlocked)
    | _ -> failwith "Mutex already unlocked"
end

module Version_Sequentielle : (S with type 'a process = ('a -> unit) -> unit) = struct 
  type 'a process = ('a -> unit) -> unit
  type 'a channel = { fd_out: Unix.file_descr;
                      fd_in: Unix.file_descr;
                      m: Mutex.t ref; }
  type 'a in_port = 'a channel
  type 'a out_port = 'a channel

  let new_channel () :'a in_port * 'a out_port = 
    let my_pipe = Unix.pipe () in
    let channel = { fd_out = snd my_pipe;
                    fd_in = fst my_pipe;
                    m = Mutex.create (); } in
    channel, channel
  
  let put (a:'a) (q:'a out_port) (c:unit -> unit) = 
    c (Mutex.lock q.m;
      let out_chan = Unix.out_channel_of_descr q.fd_out in
      Marshal.to_channel out_chan a [];
      Mutex.unlock q.m)
  let rec get (q:'a in_port) (c:'a -> unit) = 
    let in_chan = Unix.in_channel_of_descr q.fd_in in
      try
        c (
          Mutex.lock q.m;
          let value = Marshal.from_channel in_chan in
          Mutex.unlock q.m; value)
      with End_of_file ->
        Mutex.unlock q.m;
        get q c  

  let rec doco (l:unit process list)  (c:unit -> unit) = 
    match l with 
      | [] -> c ()
      | t::q -> t (fun () -> doco q c)

  let return (x:'a) (c:'a -> unit) = c x
  let bind (p1:'a process) (fp2:'a -> 'b process) (c: 'b -> unit) = p1 (fun a -> (fp2 a) c)

  let run (p:'a process) = 
    let result = ref(None) in 
    let c = (fun a -> result:= Some a) in p c;
    match !result with 
      | None -> assert false
      | Some a -> a 

end
