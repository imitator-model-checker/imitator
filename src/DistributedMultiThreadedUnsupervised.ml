(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Sami Evangelista
 * 
 * Created:       2014/07/11
 * Last modified: 2014/10/02
 *
 ****************************************************************)


open Exceptions
open ImitatorUtilities
open Options
open Reachability
open AbstractModel

(*
 *  constants
 *)

(*  max number of attempts before giving up choosing a random point  *)
let nb_tries_max = 10

(*  number of uncovered points kept by the coordinator  *)
let nb_coord_points = 100

(*  a worker sends a ping every ping_period seconds  *)
let ping_period = 0.01


(*
 *  message tags
 *)
type mtag =
    CONSTRAINT
  | ASK_FOR_POINT
  | POINT
  | PING
  | NO_CONSTRAINTS
  | TERMINATE
;;

let mtag_of_int = function
    1 -> CONSTRAINT
  | 2 -> ASK_FOR_POINT
  | 3 -> POINT
  | 4 -> PING
  | 5 -> NO_CONSTRAINTS
  | 6 -> TERMINATE
  | _ -> raise (InternalError "invalid int mtag")
;;

let int_of_mtag = function
  | CONSTRAINT     -> 1
  | ASK_FOR_POINT  -> 2
  | POINT          -> 3
  | PING           -> 4
  | NO_CONSTRAINTS -> 5
  | TERMINATE      -> 6
;;

let string_of_mtag = function
  | CONSTRAINT     -> "CONSTRAINT"
  | ASK_FOR_POINT  -> "ASK-FOR-POINT"
  | POINT          -> "POINT"
  | PING           -> "PING"
  | NO_CONSTRAINTS -> "NO-CONSTRAINTS"
  | TERMINATE      -> "TERMINATE"
;;


(*
 *  Utility functions
 *)
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

(*let valueListToPi0 (model: AbstractModel.abstract_model) l =
  let array_pi0 = Array.make model.nb_parameters NumConst.zero in
    List.iter (fun (idx, value) -> array_pi0.(idx) <- value) l;
    let pi0 = fun param -> array_pi0.(param) in
      pi0
;;*)

let string_of_node = function
| 0 -> "Coordinator"
| n -> "Worker " ^ (string_of_int n)
;;

let pr_send_msg src dest tag =
  print_message Debug_standard
    ("[" ^ (string_of_node src) ^ "] send " ^ (string_of_mtag tag) ^
       " message to " ^ (string_of_node dest))
;;

let pr_recv_msg dest src tag =
  print_message Debug_standard 
    ("[" ^ (string_of_node dest) ^ "] receive " ^ (string_of_mtag tag) ^
       " message from " ^ (string_of_node src))
;;



(*
 *  Coordinator code
 *)
let coordinator () =
  let options = Input.get_options () in
  let terminated = ref false in
  let world = Mpi.comm_world in
  let nb_workers = Mpi.comm_size world - 1 in
  let nb_workers_alive = ref nb_workers in
  let boxes = Array.make nb_workers [] in
  let update_boxes worker data =
    let update_box (i, box) =
      if i + 1 = worker
      then box
      else data :: box
    in
      array_update update_box boxes
  in
  let coordinator_init () =
    print_message Debug_standard "[Coordinator] start\n";
    Cartography.bc_initialize ();
    Cartography.compute_initial_pi0 ();
    Cartography.constraint_list_init nb_coord_points
  in    
  let coordinator_end () =
    print_message Debug_standard "[Coordinator] end\n";
    Cartography.bc_finalize ();  
    let tiles = Cartography.bc_result () in
      if options#cart then (
	Graphics.cartography (Input.get_model())
	  (Input.get_v0()) tiles (options#files_prefix ^ "_cart_patator")
      ) else (
	print_message Debug_high
	  "Graphical cartography not asked: graph not generated.";
      )
  in
  let coordinator_send worker tag data =
    pr_send_msg 0 worker tag;
    Mpi.send data worker (int_of_mtag tag) world
  in
  let coordinator_termination () =
    if !terminated
    then ()
    else (print_message Debug_standard"[Coordinator] everything is covered";
	  terminated := true)
  in
  let coordinator_send_termination worker =
    coordinator_send worker TERMINATE ();
    nb_workers_alive := !nb_workers_alive - 1
  in
  let coordinator_send_point (worker, pt) =
    let pt_str = DistributedUtilities.serialize_pi0 pt in
      coordinator_send worker POINT pt_str
  in
  let coordinator_process_ping worker =
    if !terminated
    then coordinator_send_termination worker
    else
      let i = worker - 1 in
      let b = boxes.(i) in
      let l = List.length b in
	if l = 0
	then coordinator_send worker PING ()
	else (coordinator_send worker NO_CONSTRAINTS l;
	      List.iter (fun cons -> coordinator_send
			   worker CONSTRAINT cons) b;
	      boxes.(i) <- [])
  in
  let coordinator_process_ask_for_point worker =
    match Cartography.constraint_list_random () with
      | None     -> coordinator_send_termination worker
      | Some pi0 -> coordinator_send_point (worker, pi0)
  in
  let coordinator_process_constraint (worker, data) =
    let res = DistributedUtilities.unserialize_im_result data in
      update_boxes worker data;
      let useful = Cartography.bc_process_im_result res in
	if not useful
	then ()
	else (Cartography.constraint_list_update res;
	      if not (Cartography.constraint_list_empty ())
	      then ()
	      else coordinator_termination ();
	      coordinator_process_ping worker)
  in
  let rec coordinator_loop () =
    let (d, src, tag) = Mpi.receive_status Mpi.any_source Mpi.any_tag world in
      (pr_recv_msg 0 src (mtag_of_int tag);
       match mtag_of_int tag with
	 | CONSTRAINT    -> coordinator_process_constraint (src, d)
	 | ASK_FOR_POINT -> coordinator_process_ask_for_point src
	 | PING          -> coordinator_process_ping src
	 | _             -> raise (InternalError "unexpected tag"));
      if !nb_workers_alive = 0
      then ()
      else coordinator_loop ()
  in
    try
      coordinator_init ();
      coordinator_loop ();
      coordinator_end ()
    with
      | InternalError e -> print_message Debug_standard
	  ("[Coordinator] aborted: " ^ e)
      | _ -> print_message Debug_standard ("[Coordinator] aborted")
;;



(*
 *  worker code
 *)
let worker () =
  let terminate = ref false in
  let world = Mpi.comm_world in
  let model = Input.get_model () in
  let me = Mpi.comm_rank world in
  let current_job = ref None in
  let job_result = ref None in
  let msg_prefix = "[Worker " ^ (string_of_int me) ^ "]" in
  let thread_msg_prefix = "[Worker " ^ (string_of_int me) ^
    " (thread)]" in
  let coordinator = 0 in
  let s0 = Reachability.get_initial_state_or_abort model in
  let mut = Mutex.create () in
  let worker_send data tag =
    pr_send_msg me coordinator tag;
    Mpi.send data coordinator (int_of_mtag tag) world
  in
  let worker_init () =
    print_message Debug_standard (msg_prefix ^ " start");
    Cartography.bc_initialize ()
  in
  let worker_end () =
    print_message Debug_standard (msg_prefix ^ " end")
  in
  let worker_job pi0 =
    print_message Debug_standard (thread_msg_prefix ^ " process a point");
    Input.set_pi0 pi0;
    let global_debug_mode = get_debug_mode () in
      if debug_mode_greater Debug_high
      then ()
      else set_debug_mode Debug_nodebug;
      let res, _ = Reachability.inverse_method_gen model s0 in
	set_debug_mode global_debug_mode;
	print_message Debug_standard
	  (thread_msg_prefix ^ " terminate job");
	Mutex.lock mut;
	job_result := Some (res);
	Mutex.unlock mut
  in
  let worker_launch_job pi0 =
    print_message Debug_standard (msg_prefix ^ " launch a thread");
    let t = Thread.create worker_job pi0
    in
      current_job := Some (pi0, t)
  in
  let worker_process_terminate () =
    terminate := true
  in
  let worker_process_ping () =
    ()
  in
  let worker_process_point data =
    let pi0 = DistributedUtilities.unserialize_pi0 data in
(*     let pi0 = valueListToPi0 model pi0 in *)
      if !current_job <> None
      then raise (InternalError "point received while in job")
      else worker_launch_job pi0
  in
  let worker_process_constraint data =
    let res = DistributedUtilities.unserialize_im_result data in
    let cons = res.result in
    let _ = Cartography.bc_process_im_result res in
      match !current_job with
        | None -> ()
        | Some (pt, t) ->
            if not (Cartography.pi0_in_returned_constraint pt cons)
            then ()
            else (print_message Debug_standard
                    (msg_prefix ^
                       " received constraint covers job point " ^
		       "-> kill job (not implemented)");
		  (* Sami: Thread.kill is not implemented so we have
		     to find a way to stop the current job (e.g. change
		     in the inverse method directly)

                     Thread.kill t;
                     current_job := None;
                     job_result := None
		  *)
		 )
  in
  let rec worker_process_no_constraints = function
    | 0 -> (if !current_job = None
            then worker_initiate_job ()
            else ())
    | n -> (worker_process_message ();
            worker_process_no_constraints (n - 1))
  and worker_initiate_job () =
    if Cartography.random_pi0 nb_tries_max
    then (print_message Debug_standard
            (msg_prefix ^ " has chosen a random point");
          worker_launch_job
            ((*valueListToPi0 model *)(Cartography.get_current_pi0 ())))
    else (worker_send () ASK_FOR_POINT;
          worker_process_message ())
  and worker_process_message () =
    let (data, src, tag) = Mpi.receive_status coordinator Mpi.any_tag world in
      (pr_recv_msg me src (mtag_of_int tag);
       match mtag_of_int tag with
	 | TERMINATE      -> worker_process_terminate ()
	 | CONSTRAINT     -> worker_process_constraint data
	 | POINT          -> worker_process_point data
	 | PING           -> worker_process_ping ()
	 | NO_CONSTRAINTS -> worker_process_no_constraints data
	 | _              -> raise (InternalError "unexpected tag"))
  in
  let worker_process_job_result = function
  | None -> false
  | Some cons ->
      let cons_str = DistributedUtilities.serialize_im_result cons in
      let useful = Cartography.bc_process_im_result cons in
	if not useful
	then ()
	else (worker_send cons_str CONSTRAINT;
	      worker_process_message ();
	      current_job := None;
	      job_result := None);
	true
  in
  let last_ping = ref (Sys.time ()) in
  let rec worker_loop () =
    if !terminate
    then ()
    else (Thread.yield ();                                                                                                                                                                                 
          Mutex.lock mut;
	  let job_done = worker_process_job_result (!job_result) in
	    Mutex.unlock mut;
	    if job_done
	    then (if !terminate
		  then ()
		  else (worker_initiate_job ();
			last_ping := Sys.time ();
			worker_loop ()))
	    else (let now = Sys.time () in
		    if (now -. (!last_ping)) >= ping_period
		    then (last_ping := now;
			  (*worker_send () PING;
			  worker_process_message ();
			  *)
			  worker_loop ())
		    else worker_loop ()))
  in
    try
      worker_init ();
      worker_initiate_job ();
      worker_loop ();
      worker_end ()
    with
      | InternalError e -> print_message Debug_standard
          (msg_prefix ^ " aborted: " ^ e)
      | _ -> print_message Debug_standard
	  (msg_prefix ^ " aborted")
      
;;
