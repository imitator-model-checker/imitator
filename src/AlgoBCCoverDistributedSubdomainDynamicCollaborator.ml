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
 * Last modified     : 2016/04/07
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

(* Exception when the master orders to suddenly stop an execution of IM *)
exception KillIM

(* Exception when the master sends a new subdomain while a previous one was processed *)
exception NewSubdomainAssigned of HyperRectangle.hyper_rectangle


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
	val mutable current_point : AlgoCartoGeneric.more_points = AlgoCartoGeneric.No_more

	(* The current cartography instance *)
	val mutable bc_option : AlgoBCCover.algoBCCover option = None
	
	(* Flag to discriminate between first point called and further points *)
	val mutable first_point = true

	
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
		current_point <- AlgoCartoGeneric.No_more;
		bc_option <- None;
		first_point <- true;
		
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
		
		(* Initialize *)
		bc_instance#initialize_cartography;
		
		(* Set current point *)
		current_point <- AlgoCartoGeneric.No_more;
		
		(* Set first point *)
		first_point <- true;
		
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
						
		self#print_algo_message Verbose_medium ("sent update request to the coordinator ");
		
		let killIM = ref false in
		
		let receivedContinue = ref false in
						
		while not !receivedContinue do
			let check = receive_work () in
			match check with
							
			(*** TODO/QUESTION: how can the worker receive a tile from the master here??? ***)
			| TileUpdate tile ->
				self#print_algo_error ("A worker is not supposed to receive a TileUpdate from the master here");
				raise (InternalError("A worker is not supposed to receive a TileUpdate from the master here"))
						
			| Continue ->  
				self#print_algo_message Verbose_medium ("received continue tag from the coordinator.");
				
				(* Retrieve current info *)
				(*** WARNING: safe???? ***)
				let currentPi0 = match current_point with
					| AlgoCartoGeneric.Some_pval pval -> pval
					| AlgoCartoGeneric.No_more -> raise (InternalError("Unexpected situation where no more point is found in the worker although it should be set at that point."))
				in
				
				let current_bc = a_of_a_option bc_option in
				(* Test if uncovered *)
				let uncovered = current_bc#test_pi0_uncovered currentPi0 in
				(* If not: kill *)
				(*** QUESTION: why???? ***)
				if not uncovered then killIM := true;
		
				receivedContinue := true;	
				self#print_algo_message Verbose_medium ("received Tile from the coordinator.");
										
			| _ -> self#print_algo_message Verbose_medium ("received unexpected tag at worker side in method 'check_stop_order'.");
				raise (InternalError("received unexpected tag at worker side in method 'check_stop_order'."));
							
		done; (* end while *)
		
		if !killIM then(
			raise KillIM;
		);
		
		(* The end *)
		()
	
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Compute the next point *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private compute_next_point =
	
		let bc = a_of_a_option bc_option in

		(* Print some information *)
		self#print_algo_message Verbose_low ("Computing next point...");
		
		(* Find next point (dynamic fashion) *)
		(*** NOTE: this operation (checking first point) could have been rather embedded in CartoGeneric ***)
		let next_point = 
		if first_point then(
			(* Print some information *)
			self#print_algo_message Verbose_low ("Asking BC to compute the first point...");		
			(* Unset flag *)
			first_point <- false;
			(* Call specific function *)
			bc#get_initial_point
		)else(
			(* Print some information *)
			self#print_algo_message Verbose_low ("Asking BC to compute the next point...");
			bc#compute_and_return_next_point
		)
		in
		
		(* Set point *)
		current_point <- next_point;

		(* The end *)
		()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Find the next point, process it, send the tile to the master (and possibly stop or raise NewSubdomainAssigned depending on the master orders) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private process_one_point =
		(* Retrieve the model *)
		let model = Input.get_model() in
		(* Retrieve the input options *)
		let options = Input.get_options () in
		
		(* Compute next point *)
		self#compute_next_point;
		
		(* Check nature of this point *)
		begin
		match current_point with
		(* No more point: do nothing, and the calling method (process_subdomain) will safely terminate thanks to fixpoint condition self#check_iteration_condition *)
		| AlgoCartoGeneric.No_more -> ()
		
		(* Otherwise: some work! *)
		| AlgoCartoGeneric.Some_pval pi0 ->
		
		if verbose_mode_greater Verbose_medium then(
			self#print_algo_message Verbose_medium ("pi0:");
			self#print_algo_message Verbose_medium   (ModelPrinter.string_of_pi0 model pi0);
		);
		
		(* Send to the coordinator *)
		send_point_to_master pi0;
		
		self#print_algo_message Verbose_medium ("Sent pi0 to the coordinator ");
		
		let receivedContinue = ref false in
		
		(* First communicate with the master: send the point, then ask for reply *)
		while not !receivedContinue do
	
			let check = receive_work () in
			match check with
			
			| TileUpdate tile -> self#print_algo_message Verbose_medium ("Received a tile from the coordinator.");
				(*** QUESTION: why is this line commented out ???? ***)
(* 									let b = Cartography.bc_process_im_result tile in *)
						()
						
			| Subdomain subdomain -> self#print_algo_message Verbose_medium ("received scaled subdomain tag from the coordinator.");
(*						Input.set_v0 subdomain;
						Cartography.bc_initialize_subdomain ();*)
						(*** NOTE: the master still sends a continue message after sending a subpart, so we have to wait for that message ***)
						(*** TODO: simplify the algorithm! ***)
						let check = receive_work () in
						begin
						match check with
							| Continue ->  ()
							| _ -> (self#print_algo_message Verbose_medium ("received unexpected tag at worker side in test inside method 'process_one_point'.");
								raise (InternalError("received unexpected tag at worker side in test inside method 'process_one_point'."));)
						end;
						raise (NewSubdomainAssigned subdomain)
			
			| Continue -> self#print_algo_message Verbose_medium ("received continue tag from the coordinator.");
						receivedContinue := true;	
						
			| _ -> self#print_algo_error ("received unexpected tag at worker side in method 'process_one_point'.");
				raise (InternalError("received unexpected tag at worker side in method 'process_one_point'."));
			
		done; (* end while *)
		
		
		(*** WARNING/QUESTION: why is the point not AGAIN sent to the master? Its choice may have been changed due to the new reception of tiles!!! ***)
	
	
		(* An exception KillIM can be raised by check_order *)
		
		(*** QUESTION: by whom? how? ***)
		
		(* Prepare the special termination function for the inverse method *)
		let termination_function_option =
			if options#distributedKillIM then Some (fun () -> self#check_stop_order) else None
		in
		
		try(
			(* Call IM *)
			let abstract_im_result = self#run_im pi0 termination_function_option in
			
			(* Send the result to the master *)
			DistributedUtilities.send_abstract_im_result abstract_im_result;
					
			(*** NOTE for the collaborator version: keep it in memory 
				all_tiles := im_result :: !all_tiles;
			***)
			
	(*		(* Print some info *)
			self#print_algo_message Verbose_medium (" Constraint really added? " ^ (string_of_bool added) ^ "");
			compute_next_pi0_sequentially more_pi0 limit_reached first_point (Some im_result.tile_nature);*)
		)
		with KillIM ->(
			(*** TODO: manage counter to count the number of kills ***)
			self#print_algo_message Verbose_low "\n -------------Killed IM-----------------"; 
			(* Do nothing: the next call to this function will take care of the next point, if necessary *)
		);
		
		end; (* match current_point *)
		
		(* The end *)
		()

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Process subdomain received from the master: initialize, cover it, and send all tiles to the master *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private process_subdomain subdomain =
		(* Set the subdomain *)
		(*** NOTE: would be better to have a nicer mechanism than that one… ***)
		Input.set_v0 subdomain;
		
		(* Perform initialization *)
		bc_option <- Some self#new_bc_instance;
		
		if verbose_mode_greater Verbose_medium then(
			(* Retrieve the model *)
			let model = Input.get_model() in
			self#print_algo_message Verbose_medium ("Set subdomain:");
			self#print_algo_message Verbose_medium (ModelPrinter.string_of_v0 model subdomain);
		);
		
		
		(* The following loop may raise an exception NewSubdomainAssigned, in which case we iteratively process this new subdomain *)
		try(
		
			(* While there is another point to explore *)
			while (a_of_a_option bc_option)#check_iteration_condition do
				(* Find the next point, process it *)
				self#process_one_point;
			done; (* end while more points *)
			
			(*** NOTE: No need to process the result of BC, totally useless here ***)
		
		) with
		(* If new subdomain: recursive call *)
		| NewSubdomainAssigned subdomain' ->
			self#print_algo_message Verbose_low ("I have been assigned a new subdomain!");
			self#process_subdomain subdomain'
		;
		
(*		(*initial pi0*)
		Cartography.compute_initial_pi0();
		
		self#print_algo_message Verbose_medium ("Initial pi0:");
		self#print_algo_message Verbose_medium   (ModelPrinter.string_of_pi0 model (Cartography.get_current_pi0()));
		
		let pi0 = ref (Cartography.get_current_pi0()) in
		
		while (!more_pi0 && not !limit_reached) do 			    
			
		
			
		done;
*)
	(* The end *)
	()

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Generic algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run () =
		(* Retrieve the input options *)
(* 		let options = Input.get_options () in *)
		
		(* Init counters *)
		(*** TODO ***)
(*		counter_worker_total#init;
		counter_worker_waiting#init;
		counter_worker_IM#init;
		counter_worker_find_next_pi0#init;*)
		
		(* Start global counter *)
		(*** TODO ***)
(* 		counter_worker_total#start; *)
		
		self#print_algo_message Verbose_medium ("I am worker " ^ (string_of_int collaborator_rank) ^ "/" ^ (string_of_int (nb_collaborators-1)) ^ ".");
		
		let finished = ref false in
		
		(* Initialize the cartography *)
		(*** NOTE: useful?? probably not ***)
(* 		bc <- Some self#new_bc_instance; *)
		
		
		while not !finished do
			send_work_request ();
			self#print_algo_message Verbose_medium ("Sent pull request to the master.");
			(*** TODO ***)
(* 			counter_worker_waiting#start; *)

			let work = receive_work() in
			(*** TODO ***)
(* 			counter_worker_waiting#stop; *)
			match work with
			| Subdomain subdomain -> 
				self#print_algo_message Verbose_medium ("Received subdomain from the coordinator.");
				
				self#process_subdomain subdomain;
				
			| Terminate -> 
					self#print_algo_message Verbose_medium ("I was just told to terminate work.");
					finished := true
				
			(* Otherwise: error! *)
			| Work _ -> self#print_algo_error ("received unexpected Work tag from the master in method 'run'.");
					raise (InternalError("received unexpected Work tag from the master in method 'run'."));
			| Stop -> self#print_algo_error ("received unexpected Stop tag from the master in method 'run'.");
					raise (InternalError("received unexpected Stop tag from the master in method 'run'."));
			| TileUpdate _ -> self#print_algo_error ("received unexpected TileUpdate tag from the master in method 'run'.");
					raise (InternalError("received unexpected TileUpdate tag from the master in method 'run'."));
			| Continue -> self#print_algo_error ("received unexpected Continue tag from the master in method 'run'.");
					raise (InternalError("received unexpected Continue tag from the master in method 'run'."));
			
		done;



		self#print_algo_message Verbose_low ("I am done.");
		
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
