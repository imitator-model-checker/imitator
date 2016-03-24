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
 * Last modified     : 2016/03/24
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
(* Class-independent functions *)
(************************************************************)
(************************************************************)

(*get element at*)
(*** TODO: replace with built-in function ***)
let rec at (lst : 'a list) (n : int) : 'a =
	match lst with
	h :: t -> if n = 0 then h else at t (n - 1);
	| [] -> raise Not_found;;

(*remove element at*)	
(*** TODO: rewrite and move to OCamlUtilities ***)
let rec remove_at (lst : 'a list) (n : int) : 'a list=
	match lst with
	| [] -> [];
	| h :: t -> if n = 0 then t else h :: remove_at t (n-1);;
	

(*get number of points in subpart*)
let compute_nb_points_in_subpart (s : HyperRectangle.hyper_rectangle)=
	let total = ref 1 in
	for i=0 to (HyperRectangle.get_dimensions()-1) do
	total := !total * (NumConst.to_int(s#get_max i) - NumConst.to_int(s#get_min i) + 1);
	done;
	!total
	
(*compute the how many points was done in subpart*)
let compute_nb_done_points_in_subpart (s : HyperRectangle.hyper_rectangle) (arr : AbstractModel.pi0) =
	let sum = ref 0 in
	let tail = ref 1 in
	for i= 1 to (HyperRectangle.get_dimensions()-1) do
	  for j=0 to i-1 do
	    tail := !tail*(NumConst.to_int(s#get_max j) - NumConst.to_int(s#get_min j) +1) ;
	  done;
	  sum := !sum + ( (NumConst.to_int (arr#get_value(i))) - (NumConst.to_int(s#get_min i))) * !tail;
	  tail := 1;
	done;
	!sum + (NumConst.to_int (arr#get_value(0))) - (NumConst.to_int(s#get_min 0)) + 1


let split2 s dimension n = 
	let d = HyperRectangle.get_dimensions() -1 in
	(* Sliptting subpart into 2 smaller subparts*)
	(*Display information of s*)
	(*print_message Verbose_standard ("\nSplitting............! ");
	print_message Verbose_standard ("\ns infomation: ");
	print_message Verbose_standard ("Number dimensions is : " ^ (string_of_int d) );
	for j = 0 to d do
	print_message Verbose_standard ("Dimension " ^(string_of_int j)^" : "^ " min = " ^ (string_of_int (NumConst.to_int(s#get_min j)))^";"^ " max = " ^ (string_of_int (NumConst.to_int(s#get_max j))));
	done;
	(*check pi0 in subpart*)
	let totalpi0 = getTotalPi0 s d in
	print_message Verbose_standard ("Total pi0s in s is : " ^ (string_of_int totalpi0) );*)
	(**********************************end printing***************************************************)
	let max_d_l = ref 0 in
	(*count from zero so that add 1 unit*)
	max_d_l := ( (NumConst.to_int(s#get_max dimension)) - (NumConst.to_int(s#get_min dimension)) +1 );
	if (!max_d_l = 1) then raise (InternalError ("the length is minimum, could not split smaller "));
	print_message Verbose_medium ("\ndetected Max dimension length in this subpart is : " ^ (string_of_int (!max_d_l)) ^ " unit at dimension " ^ (string_of_int (dimension))); 
	  (*create new subparts*)
	  let s1 = new HyperRectangle.hyper_rectangle in
	  let s2 = new HyperRectangle.hyper_rectangle in
	    for i = 0 to d do

	      if ( i == dimension ) 
	      then 
	      begin
	      	(if(!max_d_l > 2) 
	      	then 
	      	begin
		  (*count from zero so that substract 1 unit*)
		  s1#set_min i (s#get_min i);
		  s1#set_max i ( NumConst.numconst_of_int ((NumConst.to_int (s#get_min i)) + ((!max_d_l/n))) );
		  s2#set_min i ( NumConst.numconst_of_int ((NumConst.to_int (s#get_min i)) + (!max_d_l/n)+1) );
		  s2#set_max i (s#get_max i);
		end
		else
		begin
		  s1#set_min i (s#get_min i); 
		  s1#set_max i (s#get_min i);
		  s2#set_min i (s#get_max i); 
		  s2#set_max i (s#get_max i);
		end);
	      end
	      else 
	      begin
		s1#set_min i (s#get_min i); 
		s1#set_max i (s#get_max i);
		s2#set_min i (s#get_min i); 
		s2#set_max i (s#get_max i);
	      end
	   done;
	(*Display information of s1*)
	(*print_message Verbose_standard ("\ns1 infomation: ");
	for i = 0 to d do
	print_message Verbose_standard ("Dimension " ^(string_of_int i)^" : "^ " min = " ^ (string_of_int (NumConst.to_int(s1#get_min i)))^";"^ " max = " ^ (string_of_int (NumConst.to_int(s1#get_max i))));
	done;
	(*check pi0 in subpart*)
	let totalpi0s1 = getTotalPi0 s1 d in
	print_message Verbose_standard ("Total pi0s in s1 is : " ^ (string_of_int totalpi0s1) );
	(*Display information of s2*)
	print_message Verbose_standard ("\ns2 infomation: ");
	for i = 0 to d do
	print_message Verbose_standard ("Dimension " ^(string_of_int i)^" : "^ " min = " ^ (string_of_int (NumConst.to_int(s2#get_min i)))^";"^ " max = " ^ (string_of_int (NumConst.to_int(s2#get_max i))));
	done;
	(*check pi0 in subpart*)
	let totalpi0s2 = getTotalPi0 s2 d in
	print_message Verbose_standard ("Total pi0s in s2 is : " ^ (string_of_int totalpi0s2) );*)
	(***************************************************************************)
	[s1;s2];;
	()
	

(* dynamic split subpart *)
let dynamicSplitSubpart (s : HyperRectangle.hyper_rectangle) pi0 n : HyperRectangle.hyper_rectangle list =
	print_message Verbose_medium ("\n entering dynamic splitting process" );
	(*let pi0 = get_next_sequential_pi0_in_subpart pi0 s in *)
	let notFound = ref true in
	let max_d_l = ref 0 in
	let j = ref (HyperRectangle.get_dimensions()-1) in
	let lst = ref [] in
	while( !notFound (*&& (!j != -1)*) ) do
	  begin
	   (* if(!j = -1) then  print_message Verbose_medium ("\n all demensions of subpart could not split" ); raise (InternalError (" there are only 1 pi0 left in subpart, could not split! "));*)
	    (*if current pi0 at max dimension j but the Min at demension j of subpart is lower, split all the done pi0 below j*)
	    (*update subpart*)
	    if ( NumConst.to_int(s#get_min (!j)) < pi0.(!j) ) then begin s#set_min (!j) (NumConst.numconst_of_int (pi0.(!j))) end;
	    max_d_l := ( (NumConst.to_int(s#get_max (!j)) - pi0.(!j) ) +1 ) ;
	    (*split subpart if the remain distance from pi0 to max dimension at leat 2 points*)
	    if( !max_d_l > 1 ) then
	      begin
		print_message Verbose_medium ("\nBegin split at demension : " ^ (string_of_int (!j) ) );
		lst := split2 s !j n;
		notFound := false ;
	      end;
	    j := (!j - 1) ;
	    
	    end(*end while*)
	  done;
	    print_message Verbose_medium ("\n splitting list : " ^ (string_of_int (List.length !lst) ) );
	!lst


(* pval to array *)
let pval2array pval =
	let arr = Array.make (PVal.get_dimensions()) 0 in
	(*print_message Verbose_standard ("\nPVal dimensions : " ^ (string_of_int (PVal.get_dimensions()) ) );*)
	for i = 0 to (PVal.get_dimensions()-1) do
	  arr.(i) <- (NumConst.to_int (pval#get_value i));
	done;
	arr
	
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
	
	(* Flag to check whether the set of integers in V0 entirely is covered *)
	val mutable covered = false
	
	(* Flag to check whether early termination has been reached (due to time limit, tile limit, etc.) *)
	val mutable termination = None

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
	method run () =

		(* Retrieve the input options *)
 		let options = Input.get_options () in 
 		
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

		(* Flag to check whether the set of integers in V0 entirely is covered *)
		covered <- false;
		
		(* Flag to check whether early termination has been reached (due to time limit, tile limit, etc.) *)
		termination <- None;
		
		(*** THE ALGORITHM STARTS HERE ***)
		while not covered && termination = None do
			
			self#print_algo_message Verbose_medium ("About to receive a pull request");
			
			(*** TODO ***)
(* 			counter_master_waiting#start; *)
			let pull_request = DistributedUtilities.receive_pull_request () in
			(*** TODO ***)
(* 			counter_master_waiting#stop; *)
			
			begin
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
			| Pi0 (source_rank , pi0) -> 
				self#print_algo_message Verbose_medium ("Received a pi0 from worker " ^ (string_of_int source_rank) ^ "");
				
				(*Update pi0s*)
				if(List.mem_assoc source_rank points_buffer) then(
					points_buffer <- List.remove_assoc source_rank points_buffer;
				);
				points_buffer <- points_buffer @ [( source_rank, pi0 )];  
				self#print_algo_message Verbose_medium ("pi0s list : " ^ (string_of_int (List.length points_buffer) ) ^ "");
				
				
				(*Splitting*)
				if waiting_workers <> [] then(
					self#print_algo_message Verbose_medium ("waiting List : " ^ (string_of_int (List.length waiting_workers) ) ^ "");
					
					(*** QUESTION: what is s? ***)
					let s = List.assoc source_rank current_subparts in
					
					(*compute the remain points int this subpart*)
					
				(*** TODO ***)
(* 					counter_master_split#start; *)
					let max_size = compute_nb_points_in_subpart s in
					let done_points = compute_nb_done_points_in_subpart s pi0 in
				(*** TODO ***)
(* 					counter_master_split#stop; *)

					let remaining_points = max_size - done_points in

					self#print_algo_message Verbose_medium ("max_size " ^ (string_of_int max_size) ^ "");
					self#print_algo_message Verbose_medium ("done_points " ^ (string_of_int done_points) ^ "");
					
					self#print_algo_message Verbose_medium ("Splitting ....... ");
					
					if remaining_points > 1 (*(List.length waiting_workers)*) then(
						self#print_algo_message Verbose_medium ("Splitting ....... ");
						
						(*if splitable, remove it in the current_subparts*)
						current_subparts <- (List.remove_assoc source_rank current_subparts);
						
						let pi0arr = pval2array pi0 in
						
						(*** TODO ***)
(* 							counter_master_split#start; *)
						let newSubparts = dynamicSplitSubpart s pi0arr ((List.length waiting_workers)+1)in
						(*** TODO ***)
(* 							counter_master_split#stop; *)
						
						(*send back to this worker*)
						let subpart1 = List.hd newSubparts in
						send_subpart subpart1 source_rank;
						self#print_algo_message Verbose_medium ("sent split subpart 1....... ");
						
						current_subparts <- current_subparts @ [source_rank, subpart1];
						
						let subpart2 = (at newSubparts 1) in
						let size = compute_nb_points_in_subpart subpart2 in
						let n =
							(*** QUESTION/WARNING: why -1 ??? ***)
							if size >= (List.length waiting_workers) then (List.length waiting_workers) - 1
							(*** QUESTION/WARNING: why -1 ??? ***)
							else size - 1
						in
						
						let newSubparts2 = self#compute_initial_subdomains_with subpart2 n in
						for i = 0 to ( (List.length newSubparts2) -1) do
							let w = List.hd waiting_workers in
							send_subpart (at newSubparts2 i) w;
							current_subparts <- current_subparts @ [w, (at newSubparts2 i)];
							waiting_workers <- remove_at waiting_workers 0;
						done;
						
					) (* end if remaining_points > 1 *)
				) (* end if waiting_workers <> [] *);
				send_continue source_rank;
					
				self#print_algo_message Verbose_medium ("Received a pi0 from worker " ^ (string_of_int source_rank) ^ "");
		
			| UpdateRequest source_rank ->
				self#print_algo_message Verbose_medium ("Received UpdateRequest  ");
				
				(* Update tiles *)
				while List.mem_assoc source_rank tiles_buffer do
					DistributedUtilities.send_tileupdate (List.assoc source_rank tiles_buffer) source_rank;
					tiles_buffer <- (List.remove_assoc source_rank tiles_buffer);
					self#print_algo_message Verbose_medium ("Sent a tile to worker " ^ (string_of_int source_rank) ^ "");
				done;
				
				send_continue source_rank;

			(*0ther cases*)
			|_ -> raise (InternalError("There should be no other case here."));
			end; (* match *)

			(* Stop condition 1: no subparts? *)
			if current_subparts = [] then(
				covered <- true;
			);

			(* Stop condition 2: early termination? *)
			(*** BADPROG: code close to AlgoCartoGeneric *)
			(* Tiles limit *)
			begin
			match options#carto_tiles_limit with
				| None -> ()
				| Some limit -> if List.length abstract_im_results >= limit then(
					termination <- Some BC_Tiles_limit
				)
			end
			;
			(* Time limit *)
			begin
			match options#carto_time_limit with
				| None -> ()
				| Some limit -> if (time_from start_time) >= (float_of_int limit) then(
					termination <- Some BC_Time_limit
				)
			end
			;
			
		done; (* END WHILE *)
		
		(* Print warnings if not covered and early termination *)
		if not covered then(
			match termination with
			| None -> raise (InternalError "If not all points are covered, then early termination must have occurred.")
			| Some bc_termination -> AlgoCartoGeneric.print_warnings_limit_for bc_termination;
		);
		
		(* Final loop: stop other collaborators *)
		for i = 1 to nb_other_collaborators do
			DistributedUtilities.send_terminate i;
		done;
		
		
		self#print_algo_message Verbose_standard ("All collaborators done" );

		(* Stop global counter *)
		(*** TODO ***)
(* 		counter_master_total#stop; *)

		(* Print some information *)
		(*** TODO ***)
(*		let occupancy = (counter_master_total#value -. counter_master_waiting#value ) /. (counter_master_total#value) *. 100. in

		self#print_algo_message Verbose_standard ("Splitting time         : " ^ (string_of_float (counter_master_split#value)) ^ " s");
		self#print_algo_message Verbose_standard ("Processing time        : " ^ (string_of_float (counter_master_processing#value)) ^ " s");
		self#print_algo_message Verbose_standard ("Waiting time           : " ^ (string_of_float (counter_master_waiting#value)) ^ " s");
		self#print_algo_message Verbose_standard ("Occupancy              : " ^ (string_of_float occupancy) ^ " %");
		self#print_algo_message Verbose_standard ("wasted tiles           : " ^ (string_of_int !nb_wasted_tiles) ^ "");
		self#print_algo_message Verbose_standard ("**************************************************");
		*)

		(* Return the result *)
		self#compute_bc_result

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_bc_result : Result.imitator_result =
		(* Compute the number of points in V0 *)
		let size_v0 = (Input.get_v0 ())#get_nb_points (Input.get_options())#step in
	
		(* Get termination *)
		let termination = match termination with
			| None -> BC_Regular_termination
			| Some bc_termination -> bc_termination
		in

	
		(* Coverage is... *)
		(*** NOTE: this is only true for the original behavioral cartography; for variants this may not hold ***)
		let coverage =
			(* INTEGER COMPLETE if termination is regular and all tiles are exact or under-approximations *)
			if termination = BC_Regular_termination && (List.for_all (fun abstract_im_result -> match abstract_im_result.soundness with
					| Constraint_exact | Constraint_maybe_under -> true
					| Constraint_maybe_over | Constraint_maybe_invalid -> false
				) abstract_im_results)
				then Coverage_integer_complete
			(* UNKNOWN otherwise *)
			else Coverage_unknown
		in

		(* Return the bc_result *)
		BC_result
		{
			size_v0				= size_v0;
			
			(* List of tiles *)
			tiles				= abstract_im_results;
			
			(* Total computation time of the algorithm *)
			(*** NOTE: here the local coordinator time only ***)
			computation_time	= time_from start_time;
			
			(* Computation time to look for points *)
			find_point_time		= 0. (*** TODO ***);
			
			(* Number of points on which IM could not be called because already covered *)
			nb_unsuccessful_points= 0 (*** TODO ***);
			
			(* Evaluation of the coverage of V0 by tiles computed by the cartography *)
			coverage			= coverage;
			
			(* Termination *)
			termination			= termination;
		}


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
