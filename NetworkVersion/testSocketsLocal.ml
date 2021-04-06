(* #require "unix" *)

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

  let new_channel () =
    (* Local version *)

    (* Adresse IP locale *)
    let localhost = Unix.gethostname () in
    let host = Unix.gethostbyname localhost in
    let sockaddr = Unix.ADDR_INET (host.Unix.h_addr_list.(0), 12345) in

    (* Démarrage d'un serveur *)

    let fd_server =  Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.bind fd_server sockaddr;
    Unix.listen fd_server 10;

    (* Initiation d'un client *)
    
    let fd1 = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.connect fd1 sockaddr;

    (* Connection du client *)

    ignore(Unix.accept fd_server);
    (* Local : pas besoin de récuperer la sortie de accept *)
    
    let channel =  { fd_out = fd1;
                     fd_in = fd1;
                     m = Mutex.create (); } in
    channel, channel
    
  let put value channel () =
    Mutex.lock channel.m;
    let out_chan = Unix.out_channel_of_descr channel.fd_out in
    Marshal.to_channel out_chan value [];
    Mutex.unlock channel.m

  let rec get channel () =
    let in_chan = Unix.in_channel_of_descr channel.fd_in in
    try
      Mutex.lock channel.m;
      let value = Marshal.from_channel in_chan in
      Mutex.unlock channel.m; value
    with End_of_file ->
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
      (Printf.printf "%d\n" n;K.put n qo) >>= (fun () -> loop (n + 1))
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

module Test = Example(Version_Unix);;

Version_Unix.run Test.main;;