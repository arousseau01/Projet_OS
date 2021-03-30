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
  type 'a process
  val bind: 'a process -> ('a -> 'b process) -> 'b process
  val return: 'a -> 'a process 
end

module Instance : Monad with type 'a process = ('a -> unit) -> unit  = struct 
  type 'a process = ('a -> unit) -> unit
  let bind f k = fun c -> f (fun a -> k a c)
  let return x = fun c -> c x

  type action_type = 
    | Atom of action_type process
    | Fork of action_type*action_type
    | Stop
  
  type 'a cprocess = 'a -> action_type -> action_type
  let action_fun p = p (fun a -> Stop)
  let atom p c = Atom (bind p (fun a -> return (c a)))
  let stop c = Stop
  let par p1 p2 c = Fork (p1 c,p2 c) 
  let fork p c = Fork (action_fun p,c ())
  let rec round p_list = match p_list with 
    | [] -> return ()
    | a::a_s -> begin match a with 
      | Atom a_m -> bind a_m (fun a' -> round (List.concat [a_s;[a']]))
      | Fork (a1,a2) -> round (List.concat [a_s;[a1;a2]])
      | Stop -> round a_s
    end 

  let run p = round [action_fun p]


end














(* Exemple sombre *)
(*
module type Writer = sig
  include Monad
  val write: string -> unit process 
end

module Comput : Monad with type 'a process = 'a * string = struct
  type 'a process = 'a * string
  let bind (a,s) k = let (b,s') = k a in (b, String.concat s [s'])
  let return x = (x,"")
  let output ((a,s):'a process) = s
end

(* Pourquoi l'héritage de module ne marche pas ? *)
 module Comput_writer : Writer = struct 
  include Comput
  let write (s:string) = ((),s)
end 

(* Prendre des monades en argument *)
module type MonadTrans = sig 
  type 'a t1 (* Monade de départ *)
  type 'a t2 (* Monade de d'arrivée *)
  val lift: 'a t1 -> 'a t2 (* Transformation *)
end
*)

