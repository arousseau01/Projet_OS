
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
  
  type 'a process = (unit -> 'a)

  type 'a channel = { chan_out: out_channel;
                      chan_in: in_channel; }
  type 'a in_port = 'a channel
  type 'a out_port = 'a channel

  let return v  = (fun () -> v)

  let run p = p ()

  let bind p f = (fun () -> f (run p) ())

  let new_channel () =
    let my_pipe = Unix.pipe () in
    let channel = { chan_out = Unix.out_channel_of_descr (snd my_pipe);
                    chan_in = Unix.in_channel_of_descr (fst my_pipe); } in
    channel, channel
  
  let put value channel = 
    let out_chan =  channel.chan_out in
    (fun () ->
      Marshal.to_channel out_chan value [];
      flush out_chan;
    )

  let get channel =
    (fun () ->
      let rec next chan = 
        begin
          try
            let value = Marshal.from_channel chan  in
            value 
          with End_of_file -> next chan 
        end
      in
      next channel.chan_in
    )

  let rec doco l =
    (fun () ->
      let rec next l =
        match l with 
          | [] -> ()
          | t::q ->  begin 
            match Unix.fork () with 
              | 0 -> run t ; exit 0
              | pid -> 
              next q ; 
              ignore (Unix.wait ())
            end
      in
      next l
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

module Crible (K : S) = struct 

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
    in loop ()

  let filter (prime:int) (qi: int K.in_port) (qo:int K.out_port) : unit K.process = 
    let rec loop () = 
      ( K.get qi) >>= 
        (fun n -> if n mod prime <> 0 then (K.put n qo) >>= loop else loop ())
    in loop ()


  let rec sift (qi: int K.in_port) (qo:int K.out_port) : unit K.process = 
      (K.get qi) >>= 
        (fun prime -> (K.put prime qo) >>= 
          (fun () -> delay K.new_channel () >>= 
            (fun (q_in,q_out) -> K.doco [ filter prime qi q_out ; sift q_in qo])
          )
        )

  let main : unit K.process = 
    (delay (fun () -> K.new_channel (),K.new_channel ()) () ) >>=
      (fun ((q1_in, q1_out), (q2_in, q2_out)) -> K.doco [ integers q1_out; sift q1_in q2_out; output q2_in ])

end 

module Test = Crible(Version_Unix);;

Version_Unix.run Test.main;;

