(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: subdomain. [ACN15]
 * Coordinator algorithm
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/17
 * Last modified     : 2016/08/15
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
open Statistics


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
	

(*get number of points in subdomain*)
let compute_nb_points_in_subdomain (s : HyperRectangle.hyper_rectangle)=
	let total = ref 1 in
	for i=0 to (HyperRectangle.get_dimensions()-1) do
	total := !total * (NumConst.to_int(s#get_max i) - NumConst.to_int(s#get_min i) + 1);
	done;
	!total
	
(*compute the how many points was done in subdomain*)
let compute_nb_done_points_in_subdomain (s : HyperRectangle.hyper_rectangle) (arr : AbstractModel.pi0) =
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


(*** QUESTION: what does exactly this function do? Can it return less than 2 subdomains? More? ***)
let split2 s dimension n = 
	print_message Verbose_high ("\n entering 'split2'..." );
	let d = HyperRectangle.get_dimensions() -1 in
	(* Sliptting subdomain into 2 smaller subdomains*)
	(*Display information of s*)
	(*print_message Verbose_standard ("\nSplitting............! ");
	print_message Verbose_standard ("\ns infomation: ");
	print_message Verbose_standard ("Number dimensions is : " ^ (string_of_int d) );
	for j = 0 to d do
	print_message Verbose_standard ("Dimension " ^(string_of_int j)^" : "^ " min = " ^ (string_of_int (NumConst.to_int(s#get_min j)))^";"^ " max = " ^ (string_of_int (NumConst.to_int(s#get_max j))));
	done;
	(*check pi0 in subdomain*)
	let totalpi0 = getTotalPi0 s d in
	print_message Verbose_standard ("Total pi0s in s is : " ^ (string_of_int totalpi0) );*)
	(**********************************end printing***************************************************)
	let max_d_l = ref 0 in
	(*count from zero so that add 1 unit*)
	max_d_l := ( (NumConst.to_int(s#get_max dimension)) - (NumConst.to_int(s#get_min dimension)) +1 );
	if (!max_d_l = 1) then(
		print_error "the length is minimum, could not split smaller ";
		raise (InternalError ("the length is minimum, could not split smaller "));
	);
	print_message Verbose_high ("\ndetected Max dimension length in this subdomain is : " ^ (string_of_int (!max_d_l)) ^ " unit at dimension " ^ (string_of_int (dimension))); 
	  (*create new subdomains*)
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
	(*check pi0 in subdomain*)
	let totalpi0s1 = getTotalPi0 s1 d in
	print_message Verbose_standard ("Total pi0s in s1 is : " ^ (string_of_int totalpi0s1) );
	(*Display information of s2*)
	print_message Verbose_standard ("\ns2 infomation: ");
	for i = 0 to d do
	print_message Verbose_standard ("Dimension " ^(string_of_int i)^" : "^ " min = " ^ (string_of_int (NumConst.to_int(s2#get_min i)))^";"^ " max = " ^ (string_of_int (NumConst.to_int(s2#get_max i))));
	done;
	(*check pi0 in subdomain*)
	let totalpi0s2 = getTotalPi0 s2 d in
	print_message Verbose_standard ("Total pi0s in s2 is : " ^ (string_of_int totalpi0s2) );*)
	(***************************************************************************)
	[s1;s2];;
	()
	

(* dynamic split subdomain *)
(*** QUESTION: what does exactly this function do? Can it return less than 2 subdomains? More? ***)
let dynamicSplitSubdomain (s : HyperRectangle.hyper_rectangle) pi0 n : HyperRectangle.hyper_rectangle list =
	print_message Verbose_high ("\n entering 'dynamicSplitSubdomain'..." );
	(*let pi0 = get_next_sequential_pi0_in_subdomain pi0 s in *)
	let notFound = ref true in
	let max_d_l = ref 0 in
	let j = ref (HyperRectangle.get_dimensions()-1) in
	let lst = ref [] in
	while( !notFound (*&& (!j != -1)*) ) do
	  begin
	   (* if(!j = -1) then  print_message Verbose_medium ("\n all demensions of subdomain could not split" ); raise (InternalError (" there are only 1 pi0 left in subdomain, could not split! "));*)
	    (*if current pi0 at max dimension j but the Min at demension j of subdomain is lower, split all the done pi0 below j*)
	    (*update subdomain*)
	    if ( NumConst.to_int(s#get_min (!j)) < pi0.(!j) ) then begin s#set_min (!j) (NumConst.numconst_of_int (pi0.(!j))) end;
	    max_d_l := ( (NumConst.to_int(s#get_max (!j)) - pi0.(!j) ) +1 ) ;
	    (*split subdomain if the remain distance from pi0 to max dimension at leat 2 points*)
	    if( !max_d_l > 1 ) then
	      begin
		print_message Verbose_high ("\nBegin split at demension : " ^ (string_of_int (!j) ) );
		lst := split2 s !j n;
		notFound := false ;
	      end;
	    j := (!j - 1) ;
	    
	    end(*end while*)
	  done;
	    print_message Verbose_high ("\n splitting list : " ^ (string_of_int (List.length !lst) ) );
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
	
	(* List of abstract_point_based_results received by the coordinator *)
(* 	val mutable abstract_point_based_results : Result.abstract_point_based_result list = [] *)
	
	(* Manager for the tiles, the class of which depends on the tiles_storage type *)
	(*** NOTE: arbitrarily set to TilesManagerList, but will be initialized later anyway ***)
	(*** TODO: when the class is parameterized, will be directly set to the correct manager, without mutable ***)
	val mutable tiles_manager = new TilesManagerList.tilesManagerList

	(* Flag to check whether the set of integers in V0 entirely is covered *)
	val mutable covered = false
	
	(* Flag to check whether early termination has been reached (due to time limit, tile limit, etc.) *)
	val mutable termination = None

	(* Create an index(collaborator,supart) *)
	val mutable current_subdomains = []
	
	(* Workers in waiting list *)
	val mutable waiting_collaborators = []

	(*** QUESTION: what is this exactly? ***)
	(*** NOTE: this structure seems really inaccurate to me; TODO: improve ***)
	val mutable tiles_buffer = []
		
	(*** QUESTION: what is this exactly? ***)
	val mutable nb_wasted_tiles = 0
		
	(*** QUESTION: what is this exactly? ***)
	val mutable points_buffer = []
	
	
	
	(* Counter tracking the computation time to split subdomains *)
	val tcounter_split_subdomains = create_hybrid_counter_and_register "split subdomains" Algorithm_counter Verbose_low
	
	(* Counter tracking the computation time to process results from the collaborators *)
	val tcounter_process_result = create_hybrid_counter_and_register "process abstract results" Algorithm_counter Verbose_low

	(* Counter tracking the computation time to wait for collaborators to send requests *)
	val tcounter_wait_requests = create_hybrid_counter_and_register "wait requests" Algorithm_counter Verbose_low

	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "BC (full cov) distr DynamicSubdomain coordinator"
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
(* 		super#initialize_variables; *)
		
		(* First create the tiles manager *)
		(*** NOTE: the get function takes care of the Some/None cases (and may raise an exception if not properly initialized) ***)
		begin
		match self#get_tiles_manager_type with
			| AlgoCartoGeneric.Tiles_list -> tiles_manager <- new TilesManagerList.tilesManagerList
			| AlgoCartoGeneric.Tiles_good_bad_constraint -> raise (InternalError "not implemented yet")
		end;
		(* Now initialize the tiles manager *)
		tiles_manager#initialize;

		(* The end *)
		()


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add an abstract_point_based_result to the list of received abstract_point_based_result *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private add_abstract_point_based_result (abstract_point_based_result : abstract_point_based_result) =
		
		(* Check if already present *)
		
		(*** TODO ***)
		
		
		(* Otherwise add *)
(* 		abstract_point_based_results <- abstract_point_based_result :: abstract_point_based_results; *)
		tiles_manager#process_tile abstract_point_based_result;
		
		(* For now always add, hence return true *)
		true
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Generic algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run () =

		(* Retrieve the input options *)
 		let options = Input.get_options () in 
 		
		(* Initialize counters *)
		tcounter_split_subdomains#reset;
		tcounter_process_result#reset;
		tcounter_wait_requests#reset;
			
		(* Time counter for the algorithm *)
		start_time <- Unix.gettimeofday();

		self#print_algo_message Verbose_standard ("Hello world");
		
		(* List of subdomains maintained by the master *)
		subdomains <- [];

		(* Create an index(collaborator,supart) *)
		current_subdomains <- [];

		(* Initialize waiting list  *)
		waiting_collaborators <- [];
		
		(* Initialize list of subdomain*)
		(* Count! *)
		tcounter_split_subdomains#start;
		tcounter_split_subdomains#increment;
		subdomains <- self#compute_initial_subdomains;
		(* Count! *)
		tcounter_split_subdomains#stop;

		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			(* Retrieve the model *)
			let model = Input.get_model() in
			self#print_algo_message_newline Verbose_low ("Computed the following " ^ (string_of_int (List.length subdomains)) ^ " initial subdomains:");
			List.iteri (fun i subdomain -> 
				self#print_algo_message_newline Verbose_low ("Subdomain #" ^ (string_of_int (i+1)) ^ ":");
				self#print_algo_message Verbose_low (ModelPrinter.string_of_v0 model subdomain);
			) subdomains;
		);
		
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
			
			(* Count! *)
			tcounter_wait_requests#start;
			tcounter_wait_requests#increment;
			let pull_request = DistributedUtilities.receive_pull_request () in
			(* Count! *)
			tcounter_wait_requests#stop;
			
			begin
			match pull_request with 
			(*Pull Tag*)
			| PullOnly collaborator_rank ->
				self#print_algo_message Verbose_medium ("Received a pull request from collaborator " ^ (string_of_int collaborator_rank) ^ "");
				(* check to delete if the collaborator comeback *)
				if List.mem_assoc collaborator_rank current_subdomains then
					current_subdomains <- List.remove_assoc collaborator_rank current_subdomains;
				
				if subdomains <> [] then( 
					(*send new subdomain *)
					DistributedUtilities.send_subdomain (List.hd subdomains) collaborator_rank;
					self#print_algo_message Verbose_medium ("Sent Subdomain to collaborator " ^ (string_of_int collaborator_rank) ^ "");
					(*add into current_subdomains*)
					current_subdomains <- current_subdomains @ [( collaborator_rank, (List.hd subdomains) )];
					subdomains <- List.tl subdomains;
				)
				
				(*have not any subdomain in list -> splitting* , if could not split -> terminate*)
				(*** QUESTION: where is the split actually performed? when is the subdomain sent to the collaborator who asked for it? ***)
				else(

					waiting_collaborators <- waiting_collaborators @ [collaborator_rank];
					self#print_algo_message Verbose_high ("Worker " ^ (string_of_int collaborator_rank) ^ " go to waiting_collaborators");
					self#print_algo_message Verbose_high ("Waiting list size : " ^ (string_of_int (List.length waiting_collaborators)) );

				);
				self#print_algo_message Verbose_total ("Received a pull request from collaborator " ^ (string_of_int collaborator_rank) ^ "; end.");
				
			(*Tile Tag*)
			| Tile (collaborator_rank , abstract_point_based_result) ->
				(* Print some information *)
				if verbose_mode_greater Verbose_low then(
					(* Retrieve the model *)
					let model = Input.get_model() in
					
					self#print_algo_message Verbose_low ("Received an abstract_point_based_result from collaborator " ^ (string_of_int collaborator_rank) ^ " computed in " ^ (string_of_seconds abstract_point_based_result.computation_time)
						^ "\nConstraint:\n"
						^ (ResultProcessor.string_of_good_or_bad_constraint model.variable_names abstract_point_based_result.result)
						^ ""
					);
				);
				
				(* Check duplicated tile *)
				(* Count! *)
				tcounter_process_result#start;
				tcounter_process_result#increment;
				let constraint_added = self#add_abstract_point_based_result abstract_point_based_result in
				(* Count! *)
				tcounter_process_result#stop;

				if constraint_added then(
					(*receive abstract_point_based_result then send to the other collaborators to update*)
					for i = 0 to (List.length current_subdomains)-1 do
						if (fst (List.nth current_subdomains i)) <> collaborator_rank then(
							(*** QUESTION: why is the following line commented out? So the other collaborators are NEVER informed of a tile computation? ***)
							(* send_tile abstract_point_based_result (first (List.nth current_subdomains i));*)
							
							(*** NOTE: this structure seems really inaccurate to me; TODO: improve ***)
							tiles_buffer <- tiles_buffer @ [(fst (List.nth current_subdomains i)), abstract_point_based_result];
						);
					done
				) (* end if constraint_added *)
				(* Otherwise: the constraint was not added because it existed already *)
				else(
					nb_wasted_tiles <- nb_wasted_tiles + 1;
				);
				self#print_algo_message Verbose_total ("Received an abstract_point_based_result from collaborator " ^ (string_of_int collaborator_rank) ^ ": end.");

			(*Pi0 Tag*)
			| Pi0 (collaborator_rank , pi0) -> 
				self#print_algo_message Verbose_medium ("Received a pi0 from collaborator " ^ (string_of_int collaborator_rank) ^ "");
				
				(*Update pi0s*)
				if(List.mem_assoc collaborator_rank points_buffer) then(
					points_buffer <- List.remove_assoc collaborator_rank points_buffer;
				);
				points_buffer <- points_buffer @ [( collaborator_rank, pi0 )];  
				self#print_algo_message Verbose_medium ("pi0s list : " ^ (string_of_int (List.length points_buffer) ) ^ "");
				
				
(*				(*------------------------------------------------------------*)
				(*** NOTE: new code to test (ÉA, 01/04/2016) ***)
				(* Flag to check whether at the end of the following code, there are no remaining points *)
				let exit_with_no_remaining_points = ref false in
				(*------------------------------------------------------------*)*)
				
				(*Splitting*)
				(*** QUESTION: why splitting now? If I understand well, there are some idle collaborators, so since this collaborator has finished computing a tile from a point, then we split its current subdomain; correct? ***)
				if waiting_collaborators <> [] then(
					self#print_algo_message Verbose_medium ("waiting List : " ^ (string_of_int (List.length waiting_collaborators) ) ^ "");
					
					(*** QUESTION: what is s? Is that the subdomain currently assigned to collaborator_rank? ***)
					let s = List.assoc collaborator_rank current_subdomains in
					
					(*compute the remain points int this subdomain*)
					
					(* Count! *)
					tcounter_split_subdomains#start;
					(*** QUESTION: why two times incrementing that counter? ***)
					tcounter_split_subdomains#increment;
					let max_size = compute_nb_points_in_subdomain s in
					let done_points = compute_nb_done_points_in_subdomain s pi0 in
					(* Count! *)
					tcounter_split_subdomains#stop;

					let remaining_points = max_size - done_points in

					self#print_algo_message Verbose_high ("max_size " ^ (string_of_int max_size) ^ "");
					self#print_algo_message Verbose_high ("done_points " ^ (string_of_int done_points) ^ "");
					
					self#print_algo_message Verbose_medium ("Splitting?");
					
					if remaining_points > 1 (*(List.length waiting_collaborators)*) then(
						self#print_algo_message Verbose_medium ("Splitting ....... ");
						
						(*if splitable, remove it in the current_subdomains*)
						current_subdomains <- (List.remove_assoc collaborator_rank current_subdomains);
						
						(*** QUESTION: what is this? ***)
						let pi0arr = pval2array pi0 in
						
						(* Count! *)
						tcounter_split_subdomains#start;
						(*** QUESTION: why two times incrementing that counter? ***)
						tcounter_split_subdomains#increment;
						(*** QUESTION: why "+1"? ***)
						let newSubdomains = dynamicSplitSubdomain s pi0arr ((List.length waiting_collaborators)+1)in
						(* Count! *)
						tcounter_split_subdomains#stop;
						
						(*send back to this collaborator*)
						(*** QUESTION: what is subdomain1? ***)
						let subdomain1 = List.hd newSubdomains in
						send_subdomain subdomain1 collaborator_rank;
						(*** QUESTION: what are you exactly doing here...? ***)
						self#print_algo_message Verbose_medium ("sent split subdomain 1....... ");
						
						current_subdomains <- current_subdomains @ [collaborator_rank, subdomain1];
						
						(*** QUESTION: what is subdomain2? ***)
						let subdomain2 = (at newSubdomains 1) in
						let nb_points_in_subdomain2 = compute_nb_points_in_subdomain subdomain2 in
						(*** QUESTION: what is 'n' ? ***)
						(*** QUESTION: what is the purpose of this code? ***)
						let n =
							(*** QUESTION/WARNING: why -1 ??? ***)
							if nb_points_in_subdomain2 >= (List.length waiting_collaborators) then (List.length waiting_collaborators) - 1
							(*** QUESTION/WARNING: why -1 ??? ***)
							else nb_points_in_subdomain2 - 1
						in
						
						(*** QUESTION: what are you exactly doing here...? ***)
						let newSubdomains2 = self#compute_initial_subdomains_with subdomain2 n in
						
						(*** QUESTION: what is 'i'? ***)
						(*** WARNING: this seems to be a horrible (and quadratically costly) imperative programming style… I think a List.iteri would do better here ***)
						for i = 0 to ( (List.length newSubdomains2) -1) do
							(*** QUESTION: what is 'w'? ***)
							let w = List.hd waiting_collaborators in
							
							send_subdomain (at newSubdomains2 i) w;
							
							(*** QUESTION: what are you exactly doing here...? ***)
							current_subdomains <- current_subdomains @ [w, (at newSubdomains2 i)];
							(*** QUESTION: why 0? Are you just removing the head of waiting_collaborators? If so, there is a better way to do this ***)
							waiting_collaborators <- remove_at waiting_collaborators 0;
						done;
						
					) (* end: if remaining_points > 1 *)
					else(
						self#print_algo_message Verbose_medium ("not splitting, since remaining_points <= 1");
						(* Update flag *)
(* 						exit_with_no_remaining_points := true; *)
					)
				) (* end if waiting_collaborators <> [] *);
				
				
(*				(*------------------------------------------------------------*)
				(*** NOTE: BEGIN new try here (ÉA, 1/04/2016), as there is a bug ***)
				if !exit_with_no_remaining_points then(
	*)				self#print_algo_message Verbose_medium ("Sending Continue tag to collaborator " ^ (string_of_int collaborator_rank) ^ " after receiving a point");
					send_continue collaborator_rank;
(*				)else(
					(* Say goodbye *)
					(*** NOTE: we will still send another Terminate signal at the end of the algorithm, but the node will just not receive it; hence no worry ***)
					self#print_algo_message Verbose_medium ("Sending exceptional Terminate tag to collaborator " ^ (string_of_int collaborator_rank) ^ "");
					DistributedUtilities.send_terminate collaborator_rank;
				);
				(*** NOTE: END new try here (ÉA, 1/04/2016), as there is a bug ***)
				(*------------------------------------------------------------*)*)
					
				self#print_algo_message Verbose_total ("Received a pi0 from collaborator " ^ (string_of_int collaborator_rank) ^ ": end");
		
			| UpdateRequest collaborator_rank ->
				self#print_algo_message Verbose_medium ("Received UpdateRequest from collaborator " ^ (string_of_int collaborator_rank) ^ " ");
				
				(* Update tiles *)
				(*** QUESTION: what is 'while List.mem_assoc'???? I don't understand this syntax ***)
				while List.mem_assoc collaborator_rank tiles_buffer do
					DistributedUtilities.send_tileupdate (List.assoc collaborator_rank tiles_buffer) collaborator_rank;
					tiles_buffer <- (List.remove_assoc collaborator_rank tiles_buffer);
					self#print_algo_message Verbose_medium ("Sent a tile to collaborator " ^ (string_of_int collaborator_rank) ^ "");
				done;
				
				self#print_algo_message Verbose_medium ("Sending Continue tag to collaborator " ^ (string_of_int collaborator_rank) ^ " after receiving an update request");
				send_continue collaborator_rank;

			(*0ther cases*)
			|_ -> self#print_algo_error "There should be no other case here.";
				raise (InternalError("There should be no other case here."));
			end; (* match *)

			(* Stop condition 1: no subdomains? *)
			if current_subdomains = [] then(
				covered <- true;
			);

			(* Stop condition 2: early termination? *)
			(*** BADPROG: code close to AlgoCartoGeneric *)
			(* Tiles limit *)
			begin
			match options#carto_tiles_limit with
				| None -> ()
				| Some limit -> if (*List.length abstract_point_based_results*)tiles_manager#get_nb_results >= limit then(
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
			| None ->
				self#print_algo_error "If not all points are covered, then early termination must have occurred.";
				raise (InternalError "If not all points are covered, then early termination must have occurred.")
			| Some bc_termination -> AlgoCartoGeneric.print_warnings_limit_for bc_termination;
		);
		
		(* Final loop: stop other collaborators *)
		for collaborator_rank = 1 to nb_other_collaborators do
			self#print_algo_message Verbose_low ("Sending termination order to collaborator " ^ (string_of_int collaborator_rank));
			DistributedUtilities.send_terminate collaborator_rank;
		done;
		
		self#print_algo_message Verbose_standard ("All collaborators have been required to terminate.");

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
	
		(* Ask the tiles manager to process the result itself, by passing the appropriate arguments *)
		tiles_manager#process_result start_time size_v0 (*** TODO ***)0 termination None


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
