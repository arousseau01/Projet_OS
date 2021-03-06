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

(* Implémentation de KPN reposant sur des processus Unix communiquant via des sockets  *)
  
  type 'a process = (unit -> 'a)

  type 'a in_port = in_channel
  type 'a out_port = out_channel

  let return v  = (fun () -> v)

  let run p = p ()

  let bind p f = (fun () -> f (run p) ())

   let port = ref 12345

  let new_channel () = 

    let host = Unix.gethostbyname (Unix.gethostname ()) in
    let addr = Unix.ADDR_INET (host.Unix.h_addr_list.(0), !port) in
    port := !port + 1 ;

    let in_socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let out_socket= Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

    (* Starting server side *)
    Unix.bind out_socket addr ;
    Unix.listen out_socket 1 ;

    (* Connecting client side *)
    Unix.connect in_socket addr ;

    (* Accepting client *)
    let out_socket, _ = Unix.accept out_socket in

    (Unix.in_channel_of_descr in_socket, 
     Unix.out_channel_of_descr out_socket)
  
  let put value out_chan = 
    (fun () ->
      Marshal.to_channel out_chan value [];
      flush out_chan;
    )

  let get in_chan =
    (fun () ->
      let rec next chan = 
        begin
          try
            let value = Marshal.from_channel chan  in
            value 
          with End_of_file -> next chan 
        end
      in
      next in_chan
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