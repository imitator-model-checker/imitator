(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: subdomain. [ACN15]
 * Collaborator (non-coordinator) algorithm
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/24
 * Last modified     : 2016/03/29
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open OCamlUtilities
open ImitatorUtilities
open Exceptions
open AbstractModel
open Result
open AlgoGeneric
open DistributedUtilities


(************************************************************)
(************************************************************)
(* Local exceptions *)
(************************************************************)
(************************************************************)
exception KillIM


(************************************************************)
(************************************************************)
(* Class-independent functions *)
(************************************************************)
(************************************************************)

(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoBCCoverDistributedSubdomainDynamicCollaborator =
	object (self)
	inherit AlgoBCCoverDistributedSubdomain.algoBCCoverDistributedSubdomain as super
	
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* The current point *)
	val mutable current_point : PVal.pval option = None

	(* The current cartography instance *)
	val mutable bc : AlgoBCCover.algoBCCover option = None
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "BC (full cov) distr DynamicSubdomain collaborator#" ^ (string_of_int collaborator_rank)

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
(* 		super#initialize_variables; *)

		(* Rest instances *)
		current_point <- None;
		bc <- None;
		
		(* The end *)
		()


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Create an instance of the sequential cartography *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** NOTE: in dynamic distribution mode, since each collaborator is responsible for its own cartography, the sequential cartography is perfectly suited ***)
	method private new_bc_instance =
		let bc_instance = new AlgoBCCover.algoBCCover in
		(* Set the instance of IM / PRP that was itself set from the current cartography class *)
		bc_instance#set_algo_instance_function self#get_algo_instance_function;
		(* Return instance *)
		bc_instance


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check stop order *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** 
		New function: 
			1) check that buffer not empty
			2) if empty --> exit (nothing)
			3) if not empty, check what tag
			4) if tag = Stop then raise exception
	***)
	method private check_stop_order =
		
		send_update_request();
						
		self#print_algo_message Verbose_medium ("sent update request to master ");
		
		let killIM = ref false in
		
		let receivedContinue = ref false in
						
		while not !receivedContinue do
			let check = receive_work () in
			match check with
							
			(*** TODO: how can the worker receive a tile from the master ??? ***)
			| TileUpdate tile ->
				self#print_algo_message Verbose_medium ("received Tile from Master.");
				raise (InternalError("A worker is not supposed to receive a TileUpdate from the master here"))
						
			| Continue ->  
				self#print_algo_message Verbose_medium ("received continue tag from Master.");
				(* Retrieve current info *)
				let currentPi0 = a_of_a_option current_point in
				let current_bc = a_of_a_option bc in
				(* Test if uncovered *)
				let uncovered = current_bc#test_pi0_uncovered currentPi0 in
				(* If not: kill *)
				(*** QUESTION: why???? ***)
				if not uncovered then killIM := true;
		
				receivedContinue := true;	
				self#print_algo_message Verbose_medium ("received Tile from Master.");
										
			| _ -> 			self#print_algo_message Verbose_medium ("error!!! receive tile at worker side." ^ (*(string_of_int rank) ^*) " ");
						raise (InternalError("error!!! receive tile at worker side."));
							
		done; (* end while *)
		
		if !killIM then raise KillIM;
		
		(* The end *)
		()


	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Generic algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run () =
		(* Get the model *)
		let model = Input.get_model() in
		(* Retrieve the input options *)
		let options = Input.get_options () in
		
		(* Init counters *)
		(*** TODO ***)
(*		counter_worker_total#init;
		counter_worker_waiting#init;
		counter_worker_IM#init;
		counter_worker_find_next_pi0#init;*)
		
		(* Start global counter *)
		(*** TODO ***)
(* 		counter_worker_total#start; *)
		
		(* Assign the termination function to the inverse method *)
		if options#distributedKillIM then(
			
			(*** TODO ***)
			
(* 			Reachability.set_patator_termination_function check_stop_order; *)
		);
		
		self#print_algo_message Verbose_medium ("I am worker " ^ (string_of_int collaborator_rank) ^ "/" ^ (string_of_int (nb_collaborators-1)) ^ ".");
		
		let finished = ref false in
		
		(* In the meanwhile: compute the initial state *)
(* 		let init_state = Reachability.get_initial_state_or_abort model in *)
		
		(* Initialize the cartography *)
		(*** NOTE: useful?? probably not ***)
		bc <- Some self#new_bc_instance;
		
		(*let main_loop () = *)
		while not !finished do
			send_work_request ();
			self#print_algo_message Verbose_medium ("sent pull request to the master.");
			(*** TODO ***)
(* 			counter_worker_waiting#start; *)

			let work = receive_work() in
			(*** TODO ***)
(* 			counter_worker_waiting#stop; *)
			match work with
			| Subdomain subdomain -> 
				self#print_algo_message Verbose_medium ("received subdomain from Master.");
				
				
				(* To differentiate between initialization of pi0 / next_point *)
				let first_point = ref true in
				
				let more_pi0 = ref true in
				
				let limit_reached = ref false in
				
				(*initialize subdomain*)
				Input.set_v0 subdomain;
				
				(* Perform initialization *)
				bc <- Some self#new_bc_instance;
(* 				Cartography.bc_initialize_subdomain (); *)
				
				if verbose_mode_greater Verbose_medium then(
					self#print_algo_message Verbose_medium ("set v0:");
					self#print_algo_message Verbose_medium (ModelPrinter.string_of_v0 model subdomain);
				);
				(*
				TODO 
				(*initial pi0*)
				Cartography.compute_initial_pi0();
				
				self#print_algo_message Verbose_medium ("Initial pi0:");
				self#print_algo_message Verbose_medium   (ModelPrinter.string_of_pi0 model (Cartography.get_current_pi0()));
				
				let pi0 = ref (Cartography.get_current_pi0()) in
				
				while (!more_pi0 && not !limit_reached) do 			    
					
					self#print_algo_message Verbose_medium ("pi0:");
					self#print_algo_message Verbose_medium   (ModelPrinter.string_of_pi0 model !pi0);
					
					pi0 := (Cartography.get_current_pi0());
					
					send_pi0_worker !pi0;
					
					self#print_algo_message Verbose_medium (" send pi0 to master ");
					
					let receivedContinue = ref false in
					
					while (not !receivedContinue) do
				
						let check = receive_work () in
						match check with
						
						| TileUpdate tile -> 		self#print_algo_message Verbose_medium ("received Tile from Master.");
	(* 									let b = Cartography.bc_process_im_result tile in *)
									self#print_algo_message Verbose_medium ("received Tile from Master.");
									
						| Subdomain subdomain ->	self#print_algo_message Verbose_medium ("received scaled subdomain tag from Master.");
									Input.set_v0 subdomain;
									Cartography.bc_initialize_subdomain ();
						
						| Continue ->  		self#print_algo_message Verbose_medium ("received continue tag from Master.");
									receivedContinue := true;	
									self#print_algo_message Verbose_medium ("received Tile from Master.");
									
						| _ -> 			self#print_algo_message Verbose_medium ("error!!! receive tile at worker side." ^ (string_of_int collaborator_rank) ^ " ");
									raise (InternalError("error!!! receive tile at worker side."));
						
					done; (* end while *)
				
					(* Set the new pi0 *)
					Input.set_pi0 !pi0;
				
					(* Save verbose mode *)
					let global_verbose_mode = get_verbose_mode() in 

					
					(* Prevent the verbose messages (except in verbose mode total) *)
					if not (verbose_mode_greater Verbose_total) then
						set_verbose_mode Verbose_mute;
					
					(*** TODO ***)
(* 					counter_worker_IM#start; *)
					try(
					
					(* Compute IM *)
					(*counter_worker_IM#start;*)
					let im_result , _ = Reachability.inverse_method_gen model init_state in

					(* Get the verbose mode back *)
					set_verbose_mode global_verbose_mode;
					
					(* Process result *)
					let added = Cartography.bc_process_im_result im_result in
					(*** TODO ***)
(* 					counter_worker_IM#stop; *)
					
					(*** NOTE for the collaborator version: keep it in memory 
						all_tiles := im_result :: !all_tiles;
					***)
					
					(*send result to master*)
					send_result im_result;

					(* Print some info *)
					self#print_algo_message Verbose_medium (" Constraint really added? " ^ (string_of_bool added) ^ "");
					compute_next_pi0_sequentially more_pi0 limit_reached first_point (Some im_result.tile_nature);
					)
					with KillIM ->(
					(*** TODO ***)
						self#print_algo_message Verbose_medium "\n -------------Killed IM-----------------"; 
						compute_next_pi0_sequentially more_pi0 limit_reached first_point (None);
					);
					
					
				done;*)
				
			| Terminate -> 
					self#print_algo_message Verbose_medium (" Terminate ");
					self#print_algo_message Verbose_medium ("I was just told to terminate work.");
					finished := true
				
			| _ -> 		self#print_algo_message Verbose_medium ("error!!! not implemented.");
					raise (InternalError("not implemented."));
			
		done;



		(** THE END **)
		
		(* Stop global counter *)
		(*** TODO ***)
(* 		counter_worker_total#stop; *)

	(*	(* Print some information *)
		let occupancy = (counter_worker_total#value -. counter_worker_waiting#value ) /. (counter_worker_total#value) *. 100. in
		self#print_algo_message Verbose_medium ("I'm done.");
		self#print_algo_message Verbose_standard ("Number of unsuccessful points: " ^ (string_of_int (Cartography.get_nb_unsuccessful_points())) ^ "");
		self#print_algo_message Verbose_standard ("Waiting time                 : " ^ (string_of_float (counter_worker_waiting#value)) ^ " s");
		self#print_algo_message Verbose_standard ("Time spent on IM             : " ^ (string_of_float (counter_worker_IM#value)) ^ " s");
		self#print_algo_message Verbose_standard ("Time to find next pi0        : " ^ (string_of_float (counter_worker_find_next_pi0#value)) ^ " s");
		self#print_algo_message Verbose_standard ("Total time                   : " ^ (string_of_float (counter_worker_total#value)) ^ " s");
		self#print_algo_message Verbose_standard ("Occupancy                    : " ^ (string_of_float occupancy) ^ " %");
	*)	
		(* Return the result (here dummy) *)
		self#compute_bc_result


		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm (useless method for regular collaborator) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_bc_result =
		Distributed_worker_result


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
