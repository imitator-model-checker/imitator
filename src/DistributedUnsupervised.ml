(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Author:        Sami Evangelista, Etienne Andre
 * 
 * Created:       2014/06/10
 * Last modified: 2015/03/30
 *
 ****************************************************************)


  !!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!
 WARNING !!! THIS FILE IS NOW UNPLUGGED FROM THE IMITATOR SOURCE CODE (as of 2016/03/10)
 This paragraph should raise a compiling error (syntax error) if by any chance this file was linked from another file.
  !!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!

(**************************************************)
(* Internal modules *)
(**************************************************)
open Exceptions
open ImitatorUtilities
open Options
open AbstractModel

(*
 *  constants
 *)

(*  max number of attempts before giving up choosing a random point  *)
let nb_tries_max = 10

(*  number of uncovered points kept by the coordinator  *)
let nb_coord_points = 100



(*
 *  message tags
 *)
type mtag =
    NO_CONSTRAINTS
  | CONSTRAINT
  | ASK_FOR_POINT
  | POINT
  | CONTINUE
  | TERMINATE
;;

let int_to_mtag = function
    1 -> NO_CONSTRAINTS
  | 2 -> CONSTRAINT
  | 3 -> ASK_FOR_POINT
  | 4 -> POINT
  | 5 -> CONTINUE
  | 6 -> TERMINATE
  | _ -> raise (InternalError "invalid int mtag")
;;

let mtag_to_int = function
  | NO_CONSTRAINTS -> 1
  | CONSTRAINT     -> 2
  | ASK_FOR_POINT  -> 3
  | POINT          -> 4
  | CONTINUE       -> 5
  | TERMINATE      -> 6
;;


(*
 *  Utility functions
 *)
(*
let valueListToPi0 (model: AbstractModel.abstract_model) l =
  let array_pi0 = Array.make model.nb_parameters NumConst.zero in
    List.iter (fun (idx, value) -> array_pi0.(idx) <- value) l;
    let pi0 = fun param -> array_pi0.(param) in
      pi0
;;*)

let array_update f a =
  let len = Array.length a in
  let rec loop i =
    if i >= len
    then ()
    else (a.(i) <- f (i, a.(i));
	  loop (i + 1))
  in
    loop 0
;;


(*
 *  Coordinator code
 *)
let coordinator () =
  (* Retrieve the input options *)
(*   let options = Input.get_options () in *)

  let terminated = ref false in
  let nb_workers_done = ref 0 in
  let world = Mpi.comm_world in
  let nb_workers = Mpi.comm_size world - 1 in
  let boxes = Array.make nb_workers [] in
    
  let coordinator_init () =
    Cartography.bc_initialize ();
    Cartography.compute_initial_pi0 ();
    Cartography.constraint_list_init nb_coord_points; 
    print_message Verbose_standard"[Coordinator] start\n"
  in
    
  let coordinator_end () =
    print_message Verbose_standard("[Coordinator] end\n");
    (* Process the finalization *)
    Cartography.bc_finalize ();  
	(* Generate the graphics *)
	Cartography.output_graphical_cartography (Some "_cart_patator");
	(* The end *)
	()
  in
    
  (*------------------------------------------------------------*)
  let coordinator_send_termination worker =
    print_message Verbose_standard("[Coordinator] send termination message to worker " ^
	  (string_of_int worker));
    nb_workers_done := !nb_workers_done + 1;
    Mpi.send () worker (mtag_to_int TERMINATE) world
  in

  (*------------------------------------------------------------*)
  let coordinator_send_constraints worker =
    let idx = worker - 1 in
    let box = boxes.(idx) in
    let l = List.length box in
      print_message Verbose_standard("[Coordinator] send a bunch of " ^ (string_of_int l) ^
	    " constraints to worker " ^
	    (string_of_int worker));
      (*** QUESTION (Camille): should we send only one message? ***)
      Mpi.send l worker (mtag_to_int NO_CONSTRAINTS) world;
      List.iter
	(fun cons ->
	   print_message Verbose_standard("[Coordinator] send one constraint to worker " ^
		 (string_of_int worker));
	   Mpi.send cons worker (mtag_to_int CONSTRAINT) world)
	box;
      boxes.(idx) <- []
  in
  (*------------------------------------------------------------*)

  (*------------------------------------------------------------*)
  let coordinator_send_point (worker, pt) =
    print_message Verbose_standard("[Coordinator] send a point to worker " ^ (string_of_int worker));
    let pt_str = DistributedUtilities.serialize_pi0 pt in
      Mpi.send pt_str worker (mtag_to_int POINT) world
  in
    
  (*------------------------------------------------------------*)
  let coordinator_process_ask_for_point worker =
    match Cartography.constraint_list_random () with
      | None     -> (terminated := true;
		     coordinator_send_termination worker)
      | Some pi0 -> coordinator_send_point (worker, pi0)
  in
  (*------------------------------------------------------------*)
  let coordinator_process_received_constraint (worker, data) =
    let update_boxes res =
      let update_box (i, box) =
	if i + 1 = worker
	then box
	else res :: box
      in
	array_update update_box boxes
    in
      print_message Verbose_standard("[Coordinator] receive a constraint from worker " ^
	    (string_of_int worker));
      if !terminated
      then coordinator_send_termination worker
      else
	let res = DistributedUtilities.unserialize_im_result data in
	let useful = Cartography.bc_process_im_result res in
	  if not useful
	  then ()
	  else (Cartography.constraint_list_update res;
		if Cartography.constraint_list_empty ()
		then (terminated := true;
		      print_message Verbose_standard"[Coordinator] everything is covered";
		      coordinator_send_termination worker)
		else (update_boxes data;
		      coordinator_send_constraints worker))
  in

  (*------------------------------------------------------------*)
  let coordinator_process_termination worker =
    print_message Verbose_standard("[Coordinator] receive a termination message from worker " ^
	  (string_of_int worker));
    nb_workers_done := !nb_workers_done + 1
  in

  (*------------------------------------------------------------*)
  let rec coordinator_loop () =
    let (d, src, tag) = Mpi.receive_status Mpi.any_source Mpi.any_tag world in
      (match int_to_mtag tag with
	 | CONSTRAINT    -> coordinator_process_received_constraint (src, d)
	 | TERMINATE     -> coordinator_process_termination src
	 | ASK_FOR_POINT -> coordinator_process_ask_for_point src
	 | _ -> raise (InternalError "unexpected tag"));
      if !nb_workers_done >= nb_workers
      then ()
      else coordinator_loop ()
  in
    (*------------------------------------------------------------*)
    (* Main sequence *)
    try
      coordinator_init ();
      coordinator_loop ();
      coordinator_end ()
    with _ ->
      print_message Verbose_standard
        ("[Coordinator] aborted due to an uncatched exception")
;;



(*
 *  worker code
 *)
let worker () =
  let terminate = ref false in
  let world = Mpi.comm_world in
  let world_size = Mpi.comm_size world in
  let model = Input.get_model () in
  let me = Mpi.comm_rank world in
  let received_point : AbstractModel.pi0 option ref = ref None in
  let msg_prefix = "[Worker " ^ (string_of_int me) ^ " / " ^
    (string_of_int (world_size - 1)) ^ "]" in
  let coordinator = 0 in
  let s0 = Reachability.get_initial_state_or_abort model in
	(*------------------------------------------------------------*)
	let worker_init () =
		Cartography.bc_initialize ();
		print_message Verbose_standard(msg_prefix ^ " start\n")
	in
	(*------------------------------------------------------------*)
	let worker_end () =
		print_message Verbose_standard(msg_prefix ^ " end\n")
	in
	(*------------------------------------------------------------*)
	let worker_process_received_point data =
		let pi0 = DistributedUtilities.unserialize_pi0 data in
		received_point := Some (*valueListToPi0 model *)pi0
	in
	(*------------------------------------------------------------*)
	let worker_process_received_constraint data =
		let res = DistributedUtilities.unserialize_im_result data in
			(* ignore ( *)
		(Cartography.bc_process_im_result res)
			(* ) *)
	in
	(*------------------------------------------------------------*)
	let rec worker_process_received_constraints n =
		if n = 0
		then ()
		else (worker_wait_and_process_response ();
		worker_process_received_constraints (n - 1))
	(*------------------------------------------------------------*)
	and worker_wait_and_process_response () = 
		let (data, _, tag) = Mpi.receive_status coordinator Mpi.any_tag world in
		begin
		match int_to_mtag tag with
			| TERMINATE      -> (print_message Verbose_standard(msg_prefix ^ " receive termination message");
						terminate := true)
			| CONTINUE       -> print_message Verbose_standard(msg_prefix ^ " receive continuation message")
			| NO_CONSTRAINTS -> (print_message Verbose_standard(msg_prefix ^ " receive a bunch of contraints");
						worker_process_received_constraints data)
			| CONSTRAINT     -> (print_message Verbose_standard(msg_prefix ^ " receive a contraint");
						let _ = worker_process_received_constraint data in ())
			| POINT          -> (print_message Verbose_standard(msg_prefix ^ " receive point");
						worker_process_received_point data)
			| _         -> raise (InternalError "unexpected tag")
		end
	in
	(*------------------------------------------------------------*)
	let worker_choose_pi0 () =
		let success = Cartography.random_pi0 nb_tries_max in
			if success
			then (
				print_message Verbose_standard(msg_prefix ^ " has chosen a random point");
				Some ((*valueListToPi0 model ( *)Cartography.get_current_pi0 ()) (* ) *)
			)else (
				print_message Verbose_standard(msg_prefix ^ " send a ask-for-point to the coordinator");
				Mpi.send () coordinator (mtag_to_int ASK_FOR_POINT) world;
				worker_wait_and_process_response ();
				let res : AbstractModel.pi0 option = !received_point in
				received_point := None;
				res
			)
  in
  let worker_send_pi0 res =
    let res_str = DistributedUtilities.serialize_im_result res in
      print_message Verbose_standard(msg_prefix ^ " send a constraint to the coordinator");
      Mpi.send res_str coordinator (mtag_to_int CONSTRAINT) world;
      worker_wait_and_process_response ()
  in
  let worker_process_pi0 pi0 =
    print_message Verbose_standard(msg_prefix ^ " process a point");
    Input.set_pi0 (pi0);
    
    (* Save verbose mode *)
    let global_verbose_mode = get_verbose_mode() in 
      
      (* Prevent the verbose messages (except in verbose modes high or total) *)
      if not (verbose_mode_greater Verbose_high) then
	set_verbose_mode Verbose_mute;
      
      let res, _ = Reachability.inverse_method_gen model s0 in
	
	(* Get the verbose mode back *)
	set_verbose_mode global_verbose_mode;
	
	let useful = Cartography.bc_process_im_result res in
	  if useful
	  then worker_send_pi0 res
	  else ()
  in
  let rec worker_loop () =
    if !terminate
    then ()
    else match worker_choose_pi0 () with
      | None -> ()
      | Some pi0 -> (worker_process_pi0 pi0;
		     worker_loop ())
  in
    try
      worker_init ();
      worker_loop ();
      worker_end ()
    with _ -> let w = Mpi.comm_world in
      print_message Verbose_standard
	("[Worker " ^ (string_of_int (Mpi.comm_rank w)) ^ " / " ^
	   (string_of_int (Mpi.comm_size w - 1)) ^
	   "] due to an uncatched exception")
;;
