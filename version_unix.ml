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
             

module Version_Unix :S = struct

  open Queue
  open Mutex
  open Unix
  
  type 'a process = (unit -> 'a)
  type 'a channel = { q: 'a Queue.t; m: Mutex.t ref; }
  type 'a in_port = 'a channel
  type 'a out_port = 'a channel

  let new_channel () =
    let channel = { q = Queue.create (); m = Mutex.create (); } in
    channel, channel
  
  let put value channel () =
    Mutex.lock channel.m;
    Queue.push value channel.q;
    Mutex.unlock channel.m

  let rec get channel () =
      try
        Mutex.lock channel.m;
        let value = Queue.pop channel.q in
        Mutex.unlock channel.m;
        value 
      with Queue.Empty ->
        Mutex.unlock channel.m;
        get channel () 

  let rec doco l () =
    match l with 
      | [] -> ()
      | t::q -> begin match Unix.fork () with 
        | 0 -> t ()
        | _ -> doco q ()
      end 
  
  let return v () = v

  let bind p f () = f (p ()) ()

  let run p = p ()
end
