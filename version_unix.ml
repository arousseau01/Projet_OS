open Unix

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

module Version_Unix (*:S*) = struct 
  type 'a process = (unit -> 'a)

  type 'a channel = 'a Queue.t
  type 'a in_port = 'a channel
  type 'a out_port = 'a channel

  let new_channel () = Queue.create (), Queue.create ()
  
  let put v c () = Queue.push v c 

  let get c () = Queue.pop c

  let rec doco l () =
    match l with 
      | [] -> ()
      | t::q -> begin match fork () with 
        | 0 -> t ()
        | _ -> doco q ()
      end 
  
  let return v () = v

  let bind p f () = f (p ()) ()

  let run p = p ()
end
