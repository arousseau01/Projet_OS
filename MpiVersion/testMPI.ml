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


module Version_Unix :S = struct

(* open Marshal
   * open Unix *)
  
  type 'a process = (unit -> 'a)

  type 'a channel = int
  type 'a in_port = 'a channel
  type 'a out_port = 'a channel

  let return v  = (fun () -> v)

  let run p = p ()

  let bind p f = (fun () -> f (run p) ())

  let tag = ref 0

  let new_channel () =
    let chan = !tag in
    tag := !tag + 1;
    chan, chan
  
  let put value channel = 
    let  size = Mpi.comm_size Mpi.comm_world in
    let  rank = Mpi.comm_rank  Mpi.comm_world in 
    fun () -> 
      for dest = 0 to (size - 1) do
        if dest <> rank then begin
          Mpi.send value dest channel Mpi.comm_world; 
          end
      done
  
  let get channel =
    fun () ->
      let n = Mpi.receive Mpi.any_source channel Mpi.comm_world in 
      n

  let rec doco l =
    (fun () ->
      let  size = Mpi.comm_size Mpi.comm_world in
      let  rank = Mpi.comm_rank  Mpi.comm_world in 
      if (List.length l) > (size + 1) then begin failwith "Error: too much process for the size of the communicator\n" end;
      
      let processes = (Array.of_list l) in
      if (rank + 1) <= (Array.length processes) then begin 
          Printf.printf "%d: launching a process" rank ; print_newline () ;
          run processes.(rank) end
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
     
  let integers (qo : int K.out_port) : unit K.process = 
    let rec loop n =
      (K.put n qo) >>= (fun () -> loop (n + 1))
    in
    loop 2
    
  let output (qi : int K.in_port) : unit K.process =
    let rec loop () =
      (K.get qi) >>= (fun v -> Format.printf "%d\n" v; loop ())
    in
    loop ()
    
  let main : unit K.process =
    (delay K.new_channel ()) >>=
      (fun (q_in, q_out) -> K.doco [ integers q_out ; output q_in ; ])
end

module Test = Example(Version_Unix);;

Version_Unix.run Test.main;;
