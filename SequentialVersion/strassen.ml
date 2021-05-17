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


module Strassen (K : S) = struct

  module Lib = Lib(K)
  open Lib

  let scalar (n:int) (a:int array array) = (* renvoie n*a *)
    let a' = Array.make_matrix (Array.length a) (Array.length a.(0)) 0 in 
    for i = 0 to Array.length a - 1 do
      for j=0 to Array.length a.(0) -1 do
        a'.(i).(j) <- a.(i).(j)*n
      done;
    done;
    a'
  
  let add (a:int array array) (b:int array array) = (* renvoie a+b *)
    if Array.length a <> Array.length b || Array.length a.(0) <> Array.length b.(0) 
      then failwith "Pas la bonne taille (somme)"
    else let c = Array.make_matrix (Array.length a) (Array.length a.(0)) 0 in 
    for i = 0 to Array.length a - 1 do
      for j=0 to Array.length a.(0) -1 do
        c.(i).(j) <- a.(i).(j) + b.(i).(j)
      done;
    done;
    c

  let sub (a:int array array) (b:int array array) = (* renvoie a-b *)
    add a (scalar (-1) b)

  let cut (a:int array array) = (* Découpe a en 4 carrés de taille taille(a)/2 *)
    let na,ma = Array.length a,Array.length a.(0) in 
    let na',ma' = na/2,ma/2 in 
    let a1,a2,a3,a4 = Array.make_matrix na' ma' 0, Array.make_matrix na' ma' 0, Array.make_matrix na' ma' 0, Array.make_matrix na' ma' 0 in 
    for i=0 to na'-1 do 
      for j=0 to ma'-1 do 
        a1.(i).(j) <- a.(i).(j)
      done;
      for j= ma' to ma -1 do 
        a2.(i).(j-ma') <- a.(i).(j)
      done;
    done;
    for i=na' to na-1 do 
      for j=0 to ma'-1 do 
        a3.(i-na').(j) <- a.(i).(j)
      done;
      for j= ma' to ma -1 do 
        a4.(i-na').(j-ma') <- a.(i).(j)
      done;
    done;
    a1,a2,a3,a4

  let concat c11 c12 c21 c22 = (* Concatène 4 petits carrés en une grande matrice *)
    let n = Array.length c11 in 
    let c = Array.make_matrix (2*n) (2*n) 0 in 
    for i = 0 to n-1 do 
      for j = 0 to n-1 do 
        c.(i).(j) <- c11.(i).(j)
      done;
      for j = n to 2*n-1 do 
        c.(i).(j) <- c12.(i).(j-n)
      done;
    done;
    for i = n to 2*n-1 do 
      for j = 0 to n-1 do 
        c.(i).(j) <- c21.(i-n).(j)
      done;
      for j = n to 2*n-1 do 
        c.(i).(j) <- c22.(i-n).(j-n)
      done;
    done;
    K.return c 

  let impression (a:int array array) = (* Print une matrice *)
    K.return (
    let n,m = Array.length a, Array.length a.(0) in 
    for i=0 to n-1 do 
      for j=0 to n-1 do 
        Format.printf "%d " a.(i).(j)
      done;
      Format.printf "\n"
    done;
    )

  let rec mat_mult_p2 (a:int array array) (b:int array array) = (* Multiplie récursivement 2 matrices de taille 2^p avec l'algorithme de Strassen*)
    let na,ma,nb,mb = Array.length a,Array.length a.(0),Array.length b, Array.length b.(0) in 
    if na<>ma || na<>nb || na<>mb 
      then failwith "Pas la bonne taille (produit)"
    else if na = 1
      then K.return [|[|a.(0).(0)*b.(0).(0)|]|]
    else 
      let a11,a12,a21,a22 = cut a in 
      let b11,b12,b21,b22 = cut b in 
      let m1,m2,m3,m4,m5,m6,m7 = ref([|[||]|]),ref([|[||]|]),ref([|[||]|]),ref([|[||]|]),ref([|[||]|]),ref([|[||]|]),ref([|[||]|]) in
      let p1 = K.return (m1:=K.run (mat_mult_p2 (add a11 a22) (add b11 b22))) in 
      let p2 = K.return (m2:=K.run (mat_mult_p2 (add a21 a22) b11)) in 
      let p3 = K.return (m3:=K.run (mat_mult_p2 a11 (sub b12 b22))) in 
      let p4 = K.return (m4:=K.run (mat_mult_p2 a22 (sub b21 b11))) in 
      let p5 = K.return (m5:=K.run (mat_mult_p2 (add a11 a12) b22)) in 
      let p6 = K.return (m6:=K.run (mat_mult_p2 (sub a21 a11) (add b11 b12))) in 
      let p7 = K.return (m7:=K.run (mat_mult_p2 (sub a12 a22) (add b21 b22))) in 
      (K.doco [p1;p2;p3;p4;p5;p6;p7]) >>= (fun () -> concat (add (add !m1 !m4) (sub !m7 !m5)) (add !m3 !m5) (add !m2 !m4) (add (sub !m1 !m2) (add !m3 !m6)) )

  let rec puissance2 n = 
    if n=0 then 1 else 
    let a = (puissance2 (n/2)) in 
    if n mod 2 = 0 then a*a else a*a*2

  let log2 n = 
    let k = ref(0) in 
    let a = ref(1) in 
    while n > !a do 
      incr k; a:=2*(!a);
    done;
    !k
  
  let restrict c nc mc = (* Renvoit la matrice c.(0:nc-1).(0:mc-1) *)
    let c' = Array.make_matrix nc mc 0 in 
    for i=0 to nc-1 do 
      for j=0 to mc-1 do 
        c'.(i).(j) <- c.(i).(j)
      done;
    done;
    c'


  let mat_mult (a:int array array) (b:int array array) = (* Multiplie deux matrices par algorithme de Strassen *)
    let na,ma,nb,mb = Array.length a,Array.length a.(0),Array.length b, Array.length b.(0) in 
    if ma <> nb 
      then failwith "Pas la bonne taille (produit qcq)"
    else 
      let n = max (max na ma) (max nb mb) in 
      let k = log2 n in 
      let n' = puissance2 k in 
      let a0,b0=Array.make_matrix n' n' 0,Array.make_matrix n' n' 0 in 
      for i=0 to na-1 do
        for j=0 to ma-1 do
          a0.(i).(j) <- a.(i).(j)
        done; 
      done;
      for i=0 to nb-1 do
        for j=0 to mb-1 do
          b0.(i).(j) <- b.(i).(j)
        done; 
      done;
      (mat_mult_p2 a0 b0) >>= (fun c -> K.return (restrict c na mb))

end

let a1=[|[|1|]|]
let a2=[|
  [|1;0|];
  [|0;1|]
|]
let a3=[| 
  [|1;2;3|];
  [|4;5;6|];
  [|7;8;9|]
|]
let a4=[| 
  [|1;2;3;4|];
  [|5;6;7;8|];
  [|9;10;11;12|];
  [|13;14;15;16|]
|]

let b1=[|[|1|]|]
let b2=[| 
  [|1;0|];
  [|0;1|]
|]
let b3=[| 
  [|1;0;0|];
  [|0;1;0|];
  [|0;0;1|]
|]
let b4=[| 
  [|1;0;0;0|];
  [|0;1;0;0|];
  [|0;0;1;0|];
  [|0;0;0;1|]
|]

module Test = Strassen(VS);;

VS.run (Test.impression (VS.run (Test.mat_mult a3 b3)));;
