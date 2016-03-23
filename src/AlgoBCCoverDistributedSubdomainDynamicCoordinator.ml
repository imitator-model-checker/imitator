(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: subdomain. [ACN15]
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/17
 * Last modified     : 2016/03/23
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
(* Class definition *)
(************************************************************)
(************************************************************)
class algoBCCoverDistributedSubdomainDynamicCoordinator =
	object (self)
	inherit AlgoBCCoverDistributedSubdomain.algoBCCoverDistributedSubdomain as super
	
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* Number of collaborators (excluding the coordinator) *)
	val nb_other_collaborators = DistributedUtilities.get_nb_nodes () - 1

	(* List of subdomains maintained by the coordinator *)
	val mutable subdomains : HyperRectangle.hyper_rectangle list = []
	
	(* List of abstract_im_results received by the coordinator *)
	val mutable abstract_im_results : Result.abstract_im_result list = []
	
	(* Is the set of integers in V0 entirely covered? *)
	val mutable covered = false
	
	(* Create an index(worker,supart) *)
	val mutable current_subparts = []
	
	(* Workers in waiting list *)
	val mutable waiting_workers = []

	(*** NOTE: what is this exactly? ***)
	val mutable tiles_buffer = []
		
	(*** NOTE: what is this exactly? ***)
	val mutable nb_wasted_tiles = 0
		
	(*** NOTE: what is this exactly? ***)
	val mutable points_buffer = []

	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name =
		raise (InternalError ("not implemented"))

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
(* 		super#initialize_variables; *)
		
		(* The end *)
		()


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add an abstract_im_result to the list of received abstract_im_result *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private add_abstract_im_result abstract_im_result =
		(* Check if already present *)
		(* Otherwise add *)
		raise (InternalError ("not implemented"))
	
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Generic algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run =

		(* Initially not covered *)
		covered <- false;
		
		(* Get the model *)
	(* 	let model = Input.get_model() in *)
		(* Get the v0 *)
		let v0 = Input.get_v0() in
		
		(* Initialize counters *)
		(*** TODO ***)
	(* 	counter_master_find_nextpi0#init; *)
(*		counter_master_total#init;
		counter_master_processing#init;
		counter_master_waiting#init;
		counter_master_split#init;*)
		(*** TODO ***)
			
		(* Start total counter *)
		(*** TODO ***)
(* 		counter_master_total#start; *)
		
		self#print_algo_message Verbose_standard ("Hello world");
		
		(* Perform initialization *)
(*		counter_master_processing#start;
		Cartography.bc_initialize ();
		counter_master_processing#stop;*)
		
		(* List of subparts maintained by the master *)
		subdomains <- [];
		

		(* Create an index(worker,supart) *)
		current_subparts <- [];

		(* Initialize waiting list  *)
		waiting_workers <- [];
		
		
		(*initialize list of subpart*)
		(*** TODO ***)
(* 		counter_master_split#start; *)
		subdomains <- self#compute_initial_subdomains;
		(*** TODO ***)
(* 		counter_master_split#stop; *)
		
		(* Initialize other variables *)
		tiles_buffer <- [];
		nb_wasted_tiles <- 0;
		points_buffer <- [];

		
		(*** THE ALGORITHM STARTS HERE ***)
		while not covered do
		begin
			
			self#print_algo_message Verbose_medium ("About to receive a pull request");
			
			(*** TODO ***)
(* 			counter_master_waiting#start; *)
			let pull_request = DistributedUtilities.receive_pull_request () in
			(*** TODO ***)
(* 			counter_master_waiting#stop; *)
			
			match pull_request with 
			(*Pull Tag*)
			| PullOnly source_rank ->
					self#print_algo_message Verbose_medium ("Received a pull request from worker " ^ (string_of_int source_rank) ^ "");
					(* check to delete if the worker comeback *)
					if List.mem_assoc source_rank current_subparts then
						current_subparts <- List.remove_assoc source_rank current_subparts;
					
					if subdomains <> [] then( 
						(*send new subpart *)
						DistributedUtilities.send_subpart (List.hd subdomains) source_rank;
						self#print_algo_message Verbose_medium ("Sent Subpart to worker " ^ (string_of_int source_rank) ^ "");
						(*add into current_subparts*)
						current_subparts <- current_subparts @ [( source_rank, (List.hd subdomains) )];
						subdomains <- List.tl subdomains;
					)
					(*have not any subpart in list -> splitting* , if could not split -> terminate*)
					else(

						waiting_workers <- waiting_workers @ [source_rank];
						self#print_algo_message Verbose_high ("Worker " ^ (string_of_int source_rank) ^ " go to waiting_workers");
						self#print_algo_message Verbose_high ("Waiting list size : " ^ (string_of_int (List.length waiting_workers)) );

					);
					self#print_algo_message Verbose_medium ("Received a pull request from worker " ^ (string_of_int source_rank) ^ "; end.");
			(*Tile Tag*)
			| Tile (source_rank , abstract_im_result) ->
					self#print_algo_message Verbose_medium ("Received a abstract_im_result from worker " ^ (string_of_int source_rank) ^ "");
					(*check duplicated tile*)
						(*** TODO ***)
(* 						counter_master_processing#start; *)
					let constraint_added = self#add_abstract_im_result abstract_im_result (*Cartography.bc_process_im_result abstract_im_result*) in
						(*** TODO ***)
(* 						counter_master_processing#stop; *)

					if constraint_added then(
						(*receive abstract_im_result then send to the other workers to update*)
						for i = 0 to (List.length current_subparts)-1 do
							if (fst (List.nth current_subparts i)) <> source_rank then(
								(* send_tile abstract_im_result (first (List.nth current_subparts i));*)
								tiles_buffer <- tiles_buffer @ [(fst (List.nth current_subparts i)), abstract_im_result];
							);
						done
					) (* end if constraint_added *)
					(* Otherwise: the constraint was not added because it existed already *)
					else(
						nb_wasted_tiles <- nb_wasted_tiles + 1;
					);
					self#print_algo_message Verbose_medium ("Received a tile from worker " ^ (string_of_int source_rank) ^ "");

			(*Pi0 Tag*)
			(*| Pi0 (source_rank , pi0) -> 
					self#print_algo_message Verbose_medium ("Received a pi0 from worker " ^ (string_of_int source_rank) ^ "");
					
					(*Update pi0s*)
					if(List.mem_assoc source_rank points_buffer) then
					begin
					points_buffer <- List.remove_assoc source_rank points_buffer;
					end;
					points_buffer <- points_buffer @ [( source_rank, pi0 )];  
					self#print_algo_message Verbose_medium ("pi0s list : " ^ (string_of_int (List.length points_buffer) ) ^ "");
					
					
					(*Splitting*)
						if( not (waiting_workers = []) then
						begin
						self#print_algo_message Verbose_medium ("waiting List : " ^ (string_of_int (List.length waiting_workers) ) ^ "");
						let s = List.assoc source_rank current_subparts in
						
						(*compute the remain points int this subpart*)
						
						counter_master_split#start;
						let max_size = get_points_in_subpart s in
						let done_points = done_points_in_subpart s pi0 in
						counter_master_split#stop;

						let remain_points = max_size - done_points in

						self#print_algo_message Verbose_medium ("max_size " ^ (string_of_int max_size) ^ "");
						self#print_algo_message Verbose_medium ("done_points " ^ (string_of_int done_points) ^ "");
						
						
						self#print_algo_message Verbose_medium ("Splitting ....... ");
						if(remain_points > 1 (*(List.length waiting_workers)*)) 
						then
						begin
							self#print_algo_message Verbose_medium ("Splitting ....... ");
							(*if splitable, remove it in the current_subparts*)
							current_subparts <- (List.remove_assoc source_rank current_subparts);
							
							let pi0arr = pval2array pi0 in
							
							counter_master_split#start;
							let newSubparts = dynamicSplitSubpart s pi0arr ((List.length waiting_workers)+1)in
							counter_master_split#stop;
							
							(*send back to this worker*)
							let subpart1 = List.hd newSubparts in
							send_subpart subpart1 source_rank;
							self#print_algo_message Verbose_medium ("sent split subpart 1....... ");
							
							current_subparts <- current_subparts@[source_rank, subpart1];
							
							let subpart2 = (at newSubparts 1) in
							let size = get_points_in_subpart subpart2 in
							let n =
							(*** WARNING: why -1 ??? ***)
							if size >= (List.length waiting_workers) then (List.length waiting_workers) - 1
							(*** WARNING: why -1 ??? ***)
							else size - 1
							in
							
							let newSubparts2 = intialize_Subparts subpart2 n in
							for i = 0 to ( (List.length newSubparts2) -1) do
							let w = List.hd waiting_workers in
							send_subpart (at newSubparts2 i) w;
							current_subparts <- current_subparts@[w, (at newSubparts2 i)];
							waiting_workers <- remove_at_lst waiting_workers 0;
							done;
							
						end
						end;
						send_continue source_rank;
						
						self#print_algo_message Verbose_medium ("Received a pi0 from worker " ^ (string_of_int source_rank) ^ "");
			*)
			| UpdateRequest source_rank ->
					self#print_algo_message Verbose_medium ("Received UpdateRequest  ");
					
					(* Update tiles *)
					while(List.mem_assoc source_rank tiles_buffer) do
					begin
						DistributedUtilities.send_tileupdate (List.assoc source_rank tiles_buffer) source_rank;
						tiles_buffer <- (List.remove_assoc source_rank tiles_buffer);
						self#print_algo_message Verbose_medium ("Sent a tile to worker " ^ (string_of_int source_rank) ^ "");
					end
					done;
					
					send_continue source_rank;

			(*0ther cases*)
			|_ -> raise (InternalError("not implemented."));
		end;
		
		(**)
		if(current_subparts = []) 
		then(
			covered <- true;
			
			(*** TODO: move this to the bottom ***)
			for i = 1 to nb_other_collaborators do
				DistributedUtilities.send_terminate i;
			done
		);
		
		done; (* END WHILE *)
		
		(*
		
		
		self#print_algo_message Verbose_standard ("All workers done" );

		(* Process the finalization *)
		counter_master_processing#start;
		Cartography.bc_finalize ();
		counter_master_processing#stop;
		
		(* Stop global counter *)
		counter_master_total#stop;

		(* Print some information *)
		let occupancy = (counter_master_total#value -. counter_master_waiting#value ) /. (counter_master_total#value) *. 100. in

		self#print_algo_message Verbose_standard ("Splitting time         : " ^ (string_of_float (counter_master_split#value)) ^ " s");
		self#print_algo_message Verbose_standard ("Processing time        : " ^ (string_of_float (counter_master_processing#value)) ^ " s");
		self#print_algo_message Verbose_standard ("Waiting time           : " ^ (string_of_float (counter_master_waiting#value)) ^ " s");
		self#print_algo_message Verbose_standard ("Occupancy              : " ^ (string_of_float occupancy) ^ " %");
		self#print_algo_message Verbose_standard ("wasted tiles           : " ^ (string_of_int !nb_wasted_tiles) ^ "");
		self#print_algo_message Verbose_standard ("**************************************************");
		
		(* Generate the graphics *)
		Cartography.output_graphical_cartography (Some "_cart_patator");*)

		(* The end *)
		()
		
		;raise (InternalError ("not implemented"))

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_bc_result =
		raise (InternalError ("not implemented"))


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
