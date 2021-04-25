module type MPI = sig
    type mpi_status
    type mpi_comm
    val mpi_sucess : bool
    val mpi_init : unit -> bool
    val mpi_finalize : unit -> bool
    val mpi_comm_size : mpi_comm -> int
    val mpi_comm_rank : mpi_comm -> int
    val mpi_send : value:'a -> dest:int -> tag:int -> mpi_comm -> bool
    val mpi_receive : src:int -> tag:int -> mpi_comm -> mpi_status -> 'a
end          

module MPI (K: Kahn.S):MPI = struct
(* module MPI (K: Kahn.S) = struct *)

    type mpi_status = {
        mpi_source: int;
        mpi_tag: int;
    }

    type mpi_comm = int

    let mpi_sucess = true

    exception MPI_ERROR of string

    let _mpi_init = ref false
    let _mpi_size = ref 1
    let _mpi_rank = ref 0

    let executable = ref ""

    let channels = ref [| |]

    let _mpi_getargs () = 
        let spec = [ "-np", Arg.Set_int _mpi_size, "number of mpi processes to launch";
        "-f", Arg.Set_string executable, "name of the executable"; ] in
        let usage = ("Usage: "^Sys.argv.(0)^" -np <nb processes> -f <file>") in
        Arg.parse spec (fun _ -> ()) usage;
        let nb_CPU_max = Sys.command "./nb_CPU.sh" in
        if (!_mpi_size > nb_CPU_max) then begin
            Printf.printf "Ask for %d processes on only %d CPUs\n" !_mpi_size nb_CPU_max end
        mpi_sucess

    let _mpi_open_channels () = 
        let init_chan = K.new_channel () in 
        channels := Array.make !_mpi_size (Array.make !_mpi_size init_chan);
        for i = 1 to !_mpi_size do begin
            for j = 1 to !_mpi_size do
                let chan = K.new_channel in 
                Array.set channels.(i) j chan;
                Array.set channels.(j) i chan
                done end
            done;
        mpi_sucess

    let _mpi_split () =
        let processes = Array.init !_mpi_size (fun i () -> _mpi_rank := i; let _ = Sys.command ("./" ^ !executable)) in 
        K.doco processes;
        mpi_sucess

    let mpi_init () =

        if (!_mpi_init = true)
            raise (MPI_ERROR "MPI already started")

        if ( (_mpi_getargs ()) && (_mpi_open_channels ()) && (_mpi_split ()) ) then begin
            _mpi_init := true; mpi_sucess end
        else not mpi_sucess

    let mpi_finalize () =

        if (!_mpi_init = false)
            raise (MPI_ERROR "MPI not started")

        _mpi_init := false;
        mpi_sucess


    let mpi_comm_size comm = 
        _mpi_size

    let mpi_comm_rank comm = 
        _mpi_rank

    let mpi_send value dest tag comm =
        let chan = channels.(!rank).(dest) in 
        K.put value chan;
        mpi_sucess

    let mpi_receive src tag comm status = 
        status.src = src;
        status.tag = tag;
        let chan = channels.(!rank).(dest) in 
        K.get chan;
    
end