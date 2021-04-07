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


  let new_channel_client () =
    (* côté client *)
    let port, hostname = 
    if (Array.length Sys.argv) >= 4
    then (12345, Unix.gethostname ())
    else (int_of_string Sys.argv.(2), Sys.argv.(3))
    in
    let host = Unix.gethostbyname hostname in
    let sockaddr = Unix.ADDR_INET (host.Unix.h_addr_list.(0), port) in
    let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.connect fd sockaddr;
    let channel =  { fd_out = fd;
                     fd_in = fd;
                     m = Mutex.create (); } in
    channel, channel

  let new_channel_server () =
    (* côté serveur *)

    let localhost = Unix.gethostname () in
    let host = Unix.gethostbyname localhost in
    let port = 
      if (Array.length Sys.argv) >= 3
      then int_of_string Sys.argv.(2)
      else 12345
    in
    let sockaddr = Unix.ADDR_INET (host.Unix.h_addr_list.(0), port) in
    let fd_server =  Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.bind fd_server sockaddr;
    Unix.listen fd_server 10;
    let fd, cliend_addr = Unix.accept fd_server in
    
    let channel =  { fd_out = fd;
                     fd_in = fd;
                     m = Mutex.create (); } in
    channel, channel

  let new_channel () =
    (* retourne un canal de communication du dôté serveur ou client *)

    match Sys.argv.(1) with
    | "server" -> new_channel_server ()
    | "client" -> new_channel_client ()
    | _ -> failwith "missing argument (server / client)"


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
     
  (* let integers (qo : int K.out_port) : unit K.process = 
   *   let rec loop n =
   *     (Printf.printf "%d\n" n;K.put n qo) >>= (fun () -> loop (n + 1))
   *   in
   *   loop 2 *)
    
  let output (qi : int K.in_port) : unit K.process =
    let rec loop () =
      (K.get qi) >>= (fun v -> Format.printf "%d@." v; loop ())
    in
    loop ()
    
  let main : unit K.process =
    (delay K.new_channel ()) >>=
      (fun (q_in, q_out) -> K.doco [ output q_in ; ])
end

module Test = Example(Version_Unix);;

Version_Unix.run Test.main;;
