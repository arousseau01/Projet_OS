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

(* open Marshal
   * open Mutex
   * open Unix *)
  
  type 'a process = (unit -> 'a)
  type 'a channel = { fd_out: Unix.file_descr;
                      fd_in: Unix.file_descr;
                      m: Mutex.t ref; }
  type 'a in_port = 'a channel
  type 'a out_port = 'a channel

  let return v  = (fun () -> v)

  let run p = p ()

  let bind p f =
    (fun () -> f (run p) ())
 
  let new_channel () =
    let my_pipe = Unix.pipe () in
    let channel = { fd_out = snd my_pipe;
                    fd_in = fst my_pipe;
                    m = Mutex.create (); } in
    channel, channel
  
  let put value channel = 
    (fun () ->
      Mutex.lock channel.m;
      let out_chan = Unix.out_channel_of_descr channel.fd_out in
      Marshal.to_channel out_chan value [Marshal.Closures];
      flush out_chan;
      Mutex.unlock channel.m
    )
  let get channel =
    (fun () ->
      let in_chan = Unix.in_channel_of_descr channel.fd_in in
      let rec next chan mutex = 
        try
          Mutex.lock mutex;
          let value = Marshal.from_channel chan in
          Mutex.unlock mutex; value
        with End_of_file ->
          Mutex.unlock mutex;
          next chan mutex
          in
        next in_chan channel.m
    )

  let rec doco l =
    (fun () ->
      let rec next l =
        match l with 
          | [] -> ()
          | [t] -> run t
          | t::q ->  begin 
            match Unix.fork () with 
              | 0 -> run t ; exit 0
              | _ -> next q 
            end
      in
      next l
    )
  
end
