(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Sami Evangelista
 * 
 * Created:       2014/06/10
 * Last modified: 2014/06/14
 *
 ****************************************************************)

 
open Global
open Options
open Reachability
open AbstractModel

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
let pr = print_message Debug_standard

let valueListToPi0 (model: AbstractModel.abstract_model) l =
  let array_pi0 = Array.make model.nb_parameters NumConst.zero in
    List.iter (fun (idx, value) -> array_pi0.(idx) <- value) l;
    let pi0 = fun param -> array_pi0.(param) in
      pi0
;;

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



(******************************************************************************
 *  Coordinator code
 ******************************************************************************)
let coordinator () =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	Cartography.bc_initialize ();
  Cartography.compute_initial_pi0 ();
  let terminated = ref false in
  let nb_workers_done = ref 0 in
  let world = Mpi.comm_world in
  let nb_workers = Mpi.comm_size world - 1 in
  let boxes = Array.make nb_workers [] in
  
	let coordinator_init () = pr ("[Coordinator] start\n") in
	
	let coordinator_end () =
	pr ("[Coordinator] end\n");
	(* Process the finalization *)
	Cartography.bc_finalize ();
	
	(* Process the result and return *)
	let tiles = Cartography.bc_result () in
	(* Render zones in a graphical form *)
	if options#cart then (
		Graphics.cartography (Input.get_model()) (Input.get_v0()) tiles (options#files_prefix ^ "_cart_patator")
	) else (
		print_message Debug_high "Graphical cartography not asked: graph not generated.";
	);
	()
	
	
	in
	
	(*------------------------------------------------------------*)
  let coordinator_send_termination worker =
    pr ("[Coordinator] send termination message to worker " ^
	  (string_of_int worker));
    nb_workers_done := !nb_workers_done + 1;
    Mpi.send () worker (mtag_to_int TERMINATE) world
  in

  	(*------------------------------------------------------------*)
	let coordinator_send_constraints worker =
    let idx = worker - 1 in
    let box = boxes.(idx) in
    let l = List.length box in
      pr ("[Coordinator] send a bunch of " ^ (string_of_int l) ^
	    " constraints to worker " ^
	    (string_of_int worker));
      Mpi.send l worker (mtag_to_int NO_CONSTRAINTS) world;
      List.iter
	(fun cons ->
	   let cons_str = DistributedUtilities.serialize_im_result cons in
	     pr ("[Coordinator] send one constraint to worker " ^
		   (string_of_int worker));
	     Mpi.send cons_str worker (mtag_to_int CONSTRAINT) world)
	box;
      boxes.(idx) <- []
  in
  
	(*------------------------------------------------------------*)
  let coordinator_process_ask_for_point worker =
    let pi0 = Cartography.get_current_pi0 () in
    let pi0_str = DistributedUtilities.serialize_pi0 pi0 in
      Mpi.send pi0_str worker (mtag_to_int POINT) world
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
      pr ("[Coordinator] receive a constraint from worker " ^
	    (string_of_int worker));
      if !terminated
      then coordinator_send_termination worker
      else
	let res = DistributedUtilities.unserialize_im_result data in
	  Cartography.bc_process_im_result res;
	  if Cartography.move_to_next_uncovered_pi0 ()
	  then (update_boxes res;
		coordinator_send_constraints worker)
	  else (terminated := true;
		pr "[Coordinator] everything is covered";
		coordinator_send_termination worker)
  in
  
	(*------------------------------------------------------------*)
  let coordinator_process_termination worker =
    pr ("[Coordinator] receive a termination message from worker " ^
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
    coordinator_init ();
    coordinator_loop ();
    coordinator_end ()
;;



(******************************************************************************
 *  Worker code
 ******************************************************************************)
let worker () =
  Cartography.bc_initialize ();
  let terminate = ref false in
  let world = Mpi.comm_world in
  let world_size = Mpi.comm_size world in
  let model = Input.get_model () in
  let me = Mpi.comm_rank world in
  let received_point = ref None in
  let msg_prefix = "[Worker " ^ (string_of_int me) ^ " / " ^
    (string_of_int (world_size - 1)) ^ "]" in
  let coordinator = 0 in
  let s0 = Reachability.get_initial_state_or_abort model in
  let worker_init () =
    pr (msg_prefix ^ " start\n")
  in
  let worker_end () =
    pr (msg_prefix ^ " end\n")
  in
  let worker_process_received_point data =
    let pi0 = DistributedUtilities.unserialize_pi0 data in
      received_point := Some (valueListToPi0 model pi0)
  in
  let worker_process_received_constraint data =
    let res = DistributedUtilities.unserialize_im_result data in
      Cartography.bc_process_im_result res
  in
  let rec worker_process_received_constraints n =
    if n = 0
    then ()
    else (worker_wait_and_process_response ();
	  worker_process_received_constraints (n - 1))
  and worker_wait_and_process_response () = 
    let (data, _, tag) = Mpi.receive_status coordinator Mpi.any_tag world in
    match int_to_mtag tag with
      | TERMINATE      -> (pr (msg_prefix ^ " receive termination message");
			   terminate := true)
      | CONTINUE       -> pr (msg_prefix ^ " receive continuation message")
      | NO_CONSTRAINTS -> (pr (msg_prefix ^ " receive a bunch of contraints");
			   worker_process_received_constraints data)
      | CONSTRAINT     -> (pr (msg_prefix ^ " receive a contraint");
			   worker_process_received_constraint data)
      | POINT          -> (pr (msg_prefix ^ " receive point");
			   worker_process_received_point data)
      | _         -> raise (InternalError "unexpected tag")
  in
  let worker_choose_pi0 () =
    let success = Cartography.random_pi0 100 in
      if success
      then (pr (msg_prefix ^ " has chosen a random point");
	    Some (valueListToPi0 model (Cartography.get_current_pi0 ())))
      else (pr (msg_prefix ^ " send a ask-for-point to the coordinator");
	    Mpi.send () coordinator (mtag_to_int ASK_FOR_POINT) world;
	    worker_wait_and_process_response ();
	    let res = !received_point in
	      received_point := None;
	      res)
  in
  let worker_send_pi0 res =
    let res_str = DistributedUtilities.serialize_im_result res in
      pr (msg_prefix ^ " send a constraint to the coordinator");
      Mpi.send res_str coordinator (mtag_to_int CONSTRAINT) world;
      worker_wait_and_process_response ()
  in
  let worker_process_pi0 pi0 =
    pr (msg_prefix ^ " process a point");
    Input.set_pi0 (pi0);
    
		(* Save debug mode *)
		let global_debug_mode = get_debug_mode() in 
			
		(* Prevent the debug messages (except in verbose modes high or total) *)
		if not (debug_mode_greater Debug_high) then
				set_debug_mode Debug_nodebug;
			
		let res, _ = Reachability.inverse_method_gen model s0 in
		
		(* Get the debug mode back *)
		set_debug_mode global_debug_mode;
		
		Cartography.bc_process_im_result res;
		worker_send_pi0 res
  in
  let rec worker_loop () =
    if !terminate
    then ()
    else match worker_choose_pi0 () with
      | None -> ()
      | Some pi0 -> (worker_process_pi0 pi0;
		     worker_loop ())
  in
    worker_init ();
    worker_loop ();
    worker_end ();
;;
