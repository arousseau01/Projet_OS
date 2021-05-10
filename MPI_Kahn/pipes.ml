module K : Kahn.S = struct

(* Implémentation de KPN reposant sur des processus Unix communiquant via des pipes  *)

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