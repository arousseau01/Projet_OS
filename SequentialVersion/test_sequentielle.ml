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
      (K.get qi) >>= (fun v -> Format.printf "%d@." v; loop ())
    in
    loop ()
    
  let main : unit K.process =
    (delay K.new_channel ()) >>=
      (fun (q_in, q_out) -> K.doco [ integers q_out ; output q_in ; ])
end

module Test = Example(Version_Sequentielle);;

Version_Sequentielle.run Test.main;;