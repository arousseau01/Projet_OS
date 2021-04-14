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
  
  type 'a process = 'a Lazy.t
  type 'a channel = { fd_out: Unix.file_descr;
                      fd_in: Unix.file_descr;
                      m: Mutex.t ref; }
  type 'a in_port = 'a channel
  type 'a out_port = 'a channel

  let return v = lazy ( v )

  let bind p f = lazy (Lazy.force (f (Lazy.force (p))))

  let run p = Lazy.force p

  let new_channel () =
    let my_pipe = Unix.pipe () in
    let channel = { fd_out = snd my_pipe;
                    fd_in = fst my_pipe;
                    m = Mutex.create (); } in
    channel, channel
  
  let put value channel =
    lazy (
    Mutex.lock channel.m;
    let out_chan = Unix.out_channel_of_descr channel.fd_out in
    Marshal.to_channel out_chan value [];
    Mutex.unlock channel.m
    ) 

  let rec get channel =
      let in_chan = Unix.in_channel_of_descr channel.fd_in in
        try
          Mutex.lock channel.m;
          let value = Marshal.from_channel in_chan in
          Mutex.unlock channel.m; return value

        with End_of_file -> 
          Mutex.unlock channel.m;
          get channel

  let doco l =
    lazy (
      let rec doco_loop l =
        match l with 
          | [] -> ()
          | t::q -> print_endline "forking a process...";
          begin match Unix.fork () with 
            | 0 -> print_endline "I'm a son"; run t 
            | _ -> doco_loop q 
          end 
          in
          doco_loop l
    )
  

end

module Lib (K : S) = struct
  let ( >>= ) x f = K.bind x f
  let delay f x =
    (K.return ()) >>= (fun () -> K.return (f x))
end

module Example (K : S) = struct
  
  module Lib = Lib(K)
  open Lib
     
  let integers (qo : int K.out_port) =
      print_endline "entering integers";
      let rec loop n =
        (K.put n qo) >>= (fun () -> loop (n + 1))
      in
      ignore (loop 2) 
    
  let output (qi : int K.in_port)  =
      print_endline "entering output";
      let rec loop () =
        print_endline "looping in output";
        (K.get qi) >>= (fun v -> Printf.printf "I'm output : %d@." v; loop ())
      in
      ignore (loop ()) 

  let main : unit K.process =
    lazy (
      print_endline "entering main";
      (delay K.new_channel ()) >>=
        (fun (q_in, q_out) -> K.doco [ lazy (integers q_out) ; lazy (output q_in) ; ])
    )
end

module Test = Example(Version_Unix);;

Version_Unix.run Test.main;;