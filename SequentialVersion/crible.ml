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


module VS : S  = (* Version Séquentielle *)
  struct
    let actions : (unit -> unit) Queue.t = Queue.create () (* Ensemble des actions à effectuer sous forme de queue *)

    let suivant () = (* Exécute la première action de la queue *)
	    if Queue.is_empty actions
	    then ()
	    else Queue.pop actions ()
    let add c = Queue.push c actions (* Ajoute une action à la queue *)

    type 'a process = ('a -> unit) -> unit
    type 'a in_port = 'a Queue.t
    type 'a out_port = 'a Queue.t

    let new_channel () =
      let q = Queue.create () in (q, q)

    (* c désigne une continuation *)
    let put (a : 'a) (q : 'a out_port) (c : unit -> unit) = 
      (* On ajoute à la queue l'action consistant à ajouter l'élément a à q puis à effectuer la continuation *)
      add (fun () -> Queue.push a q; c ()) ; 
      (* Puis on exécute l'action suivante *)
      suivant ()

    let rec get (q : 'a in_port) (c : 'a -> unit) =
	    (* On ajoute à la queue l'action consitant à appliquer la continuation au premier élément de q *)
      add
	      (fun () ->
	        if Queue.is_empty q
          (* Si q est vide, on ajoute à la queue l'action get q c. Avant que celle-ci soit effectuée à nouveau, toutes les autres actions auront été effectuée, ce qui permet d'attendre que q contienne un élément *)
	        then get q c
	        else c (Queue.pop q)) ;
      suivant ()

    let doco (l : unit process list) (c : unit -> unit) =
      (* On ajoute toutes les actions à faire à la queue, on termine la queue puis on fait la continuation *)
      List.iter (fun p -> add (fun () -> p suivant)) l ;
      ignore (suivant ()) ;
      c ()

    let return (a:'a) (c:'a -> unit) = c a
    let bind (p1 : 'a process) (fp2 : 'a -> 'b process) (c : 'b -> unit) = p1 (fun a -> (fp2 a) c)

    let run p = 
      (* Comme on a pas accès directement aux valeurs renvoyées par un processus, on se sert de la continuation pour modifier une référence *)
      let result = ref(None) in 
      let c = (fun a -> result:= Some a; suivant ()) in p c;
      match !result with 
        | None -> assert false
        | Some a -> a 

 end

module Lib (K : S) = struct
  let ( >>= ) x f = K.bind x f
  let delay f x =
    (K.return ()) >>= (fun () -> K.return (f x))
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

module Test = Crible(VS);;

VS.run Test.main;;