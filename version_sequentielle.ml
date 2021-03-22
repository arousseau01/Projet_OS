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

module type Monad = sig 
  type 'a m
  val star: 'a m -> ('a -> 'b m) -> 'b m
  val return: 'a -> 'a m 
end

module type Writer = sig
  include Monad
  val write: string -> unit m 
end

module Comput : Monad = struct
  type 'a m = 'a * string
  let star (a,s) k = let (b,s') = k a in (b, String.concat s [s'])
  let return x = (x,"")
  let output ((a,s):'a m) = s
end

(* Pourquoi l'héritage de module ne marche pas ? *)
 module Comput_writer : Writer = struct 
  (* include Comput*)
  type 'a m = 'a * string
  let star (a,s) k = let (b,s') = k a in (b, String.concat s [s'])
  let return x = (x,"")
  let write (s:string) = ((),s)
end 

(* Prendre des monades en argument *)
module type MonadTrans = sig 
  type 'a t1 (* Monade de départ *)
  type 'a t2 (* Monade de d'arrivée *)
  val lift: 'a t1 -> 'a t2 (* Transformation *)
end


