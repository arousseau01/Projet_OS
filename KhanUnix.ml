open Unix (*http://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html*)

module Khan = struct

  (* INTERFACE *)
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

  (* IMPLEMENTATION *)

  module Th : S = struct

    (* processus = tuple des listes de canaux d'entrées et de sorties associée a une fonction ayant pour paramètres ces canaux et retournant void *)

    type 'a channel = Unix.file_descr * Unix.file_descr
    type 'a in_port = Unix.file_descr
    type 'a out_port = Unix.file_descr
    type 'a process = ('a out_port list * 'a in_port list) * (('a out_port list * 'a in_port list) -> unit)


    (* variable pour les noms de fifo *)
    let nombre_canaux = ref 0

                      
    let new_channel () =
      (* cré une pipe nommée et retourne des descipteurs de fichier sur son entrée et sa sortie *)
      nombre_canaux := !nombre_canaux + 1;
      let name = "channels" ^ (string_of_int !nombre_canaux) in
      Unix.mkfifo name 0o666;
      let fd_out = Unix.openfile "fifo" [O_WRONLY] 0o666 in
      let fd_in = Unix.openfile "fifo" [O_RDONLY] 0o666 in
      (fd_out, fd_in)

      
    let run proc =
      (* cré un nouveau processus Unix et y éxécute la fonction avec comme paramètre les canaux associés, retourne le pid du processus*)
      match Unix.fork () with
      | 0 -> snd proc (fst proc)
      | pid -> pid

             
    let put value fd_out =
      (* cré un processus fils chargé d'écrire puis de quitter et vérifie qu'il a quitté normalement *)
      let pid = run (([fd_out], []),
           fun channels ->
           let out_c = Unix.out_channel_of_descr (List.hd (fst channels)) in
           Printf.fprintf out_c "%d\n" value; exit 0) in
      match snd (Unix.waitpid [] pid) with
      | WEXITED x -> if (x == 0) then (([],[]),fun channels -> ()) else failwith "Erreur à l'écriture"
      | _ -> failwith "Erreur à l'écriture"

           
    let get fd_in =
      (* cré un processus fils chargé de lire dans fd_in puis de quitter, on récupère la valeur lue dans la valeur d'exit *)
      let pid = run (([], [fd_in]),
           fun channels ->
           let in_c = Unix.in_channel_of_descr (List.hd (snd channels)) in
           let value = int_of_string (input_line in_c) in exit value ) in
      match snd (Unix.waitpid [] pid) with
      | WEXITED x -> x
      | _ -> failwith "Erreur à l'écriture"
           
                              

    let return value =
      (*cré un processus put, écrivant value dans stdout et terminant *)
      put value Unix.stdout

      
    let bind proc1 proc2 =
      (* cré un canal et l'ajoute dans la liste des canaux de sortie du processus 1, et dans la liste des canaux d'entrées du processus 2 *)
      let channel = new_channel () in
      ignore(fst (fst proc1) = (fst (fst proc1)) @ [channel]);
      ignore(snd (fst proc1) = (snd (fst proc1)) @ [channel])

      
    let doco list_process =
      (* renvoi un processus éxécutant la liste des processus *)
      (([],[]), fun channels -> List.iter(fun process -> ignore(run process)) list_process; ())

  end

end;;
                
