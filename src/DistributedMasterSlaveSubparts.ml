(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre, Camille Coti, Hoang Gia Nguyen
 * 
 * Created:       2014/09/05
 * Last modified: 2014/09/22
 *
 ****************************************************************)

 
open Global
open Options
open Mpi
open Reachability
open DistributedUtilities
(*Exception*)
exception Ex of string;;


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Counters *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(*------------------------------------------------------------*)
(* Slaves *)
(*------------------------------------------------------------*)

let counter_master_waiting 		= new Counter.counter
let counter_master_find_nextpi0	= new Counter.counter
let counter_worker_waiting 		= new Counter.counter
let counter_worker_working 		= new Counter.counter


(****************************************************************)
(**     MASTER      *)
(****************************************************************)
(*get element at*)	
let rec at (lst : HyperRectangle.hyper_rectangle list) (n : int) : HyperRectangle.hyper_rectangle =
	match lst with
	h :: t -> if n = 0 then begin h end else begin at t (n - 1) end;
	| [] -> raise Not_found;;

(*remove element at*)	
let rec remove_at (lst : HyperRectangle.hyper_rectangle list) (n : int) : HyperRectangle.hyper_rectangle list=
	match lst with
	| [] -> [];
	| h :: t -> if n = 0 then t else h :: remove_at t (n-1);;
    
(*get total pi0 inside subpart put the subpart and number of dimensions then get the number of the pi0s inside it*) 
let getTotalPi0 (subpart : HyperRectangle.hyper_rectangle) (d : int) = 
	let temp = ref 1 in
	for i = 0 to d do
	  temp := !temp*( (NumConst.to_int(subpart#get_max i) - NumConst.to_int(subpart#get_min i) ) +1);
	done ;  !temp;;

let split s dimension = 
	let d = HyperRectangle.get_dimensions() -1 in
	(* Sliptting subpart into 2 smaller subparts*)
	(*Display information of s*)
	print_message Debug_standard ("\nSplitting............! ");
	print_message Debug_standard ("\ns infomation: ");
	print_message Debug_standard ("Number dimensions is : " ^ (string_of_int d) );
	for j = 0 to d do
	print_message Debug_standard ("Dimension " ^(string_of_int j)^" : "^ " min = " ^ (string_of_int (NumConst.to_int(s#get_min j)))^";"^ " max = " ^ (string_of_int (NumConst.to_int(s#get_max j))));
	done;
	(*check pi0 in subpart*)
	let totalpi0 = getTotalPi0 s d in
	print_message Debug_standard ("Total pi0s in s is : " ^ (string_of_int totalpi0) );
	(**********************************end printing***************************************************)
	let max_d_l = ref 0 in
	(*count from zero so that add 1 unit*)
	max_d_l := ( (NumConst.to_int(s#get_max dimension)) - (NumConst.to_int(s#get_min dimension)) +1 );
	if (!max_d_l = 1) then raise (Ex ("the length is minimum, could not split smaller "));
	print_message Debug_standard ("\ndetected Max dimension length in this subpart is : " ^ (string_of_int (!max_d_l)) ^ " unit at dimension " ^ (string_of_int (dimension))); 
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
		  s1#set_max i ( NumConst.numconst_of_int ((NumConst.to_int (s#get_min i)) + ((!max_d_l/2)-1)) );
		  s2#set_min i ( NumConst.numconst_of_int ((NumConst.to_int (s#get_min i)) + (!max_d_l/2)) );
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
	print_message Debug_standard ("\ns1 infomation: ");
	for i = 0 to d do
	print_message Debug_standard ("Dimension " ^(string_of_int i)^" : "^ " min = " ^ (string_of_int (NumConst.to_int(s1#get_min i)))^";"^ " max = " ^ (string_of_int (NumConst.to_int(s1#get_max i))));
	done;
	(*check pi0 in subpart*)
	let totalpi0s1 = getTotalPi0 s1 d in
	print_message Debug_standard ("Total pi0s in s1 is : " ^ (string_of_int totalpi0s1) );
	(*Display information of s2*)
	print_message Debug_standard ("\ns2 infomation: ");
	for i = 0 to d do
	print_message Debug_standard ("Dimension " ^(string_of_int i)^" : "^ " min = " ^ (string_of_int (NumConst.to_int(s2#get_min i)))^";"^ " max = " ^ (string_of_int (NumConst.to_int(s2#get_max i))));
	done;
	(*check pi0 in subpart*)
	let totalpi0s2 = getTotalPi0 s2 d in
	print_message Debug_standard ("Total pi0s in s2 is : " ^ (string_of_int totalpi0s2) );
	(***************************************************************************)
	[s1;s2];;
	()

(* Slipt subpart put HyperRectangle s return List [s1;s2] !! the number of pi0 in subpart must be larger or equals 2*)
let sliptLongestDimensionSubpart (s : HyperRectangle.hyper_rectangle) =
	let d = HyperRectangle.get_dimensions() -1 in
	(* Sliptting subpart into 2 smaller subparts*)
	let max_d_l = ref 0 in
	let max_d = ref 0 in
	let temp = ref 0 in
	for i = 0 to d do
	  (*count from zero so that add 1 unit*)
	  temp := ( (NumConst.to_int(s#get_max i)) - (NumConst.to_int(s#get_min i)) +1 );
	  (*it will take the longest dimension to split note that = is for priority, to optimize for splitting on the fly it will depend on which demension is incearse first! *)
	  if !temp >= !max_d_l 
	    then 
	    begin
	      max_d_l := !temp; 
	      max_d := i ;
	    end
	done;
	
	let listSubpart = split s !max_d in

	listSubpart;;
	()
	
(*initial Subparts function: put in the Subpart list with number of the Workers return the new split subpart list*)
let intialize_Subparts (s : HyperRectangle.hyper_rectangle list) (n : int) =
	let subparts = ref s in
	for l = 0 to n do 
	begin
	
 (*find the largest subpart to slipt*)
	  let max_pi0s = ref 0 in
	  let subno = ref 0 in
	  for i = 0 to (List.length !subparts)-1 do
	      let temp = getTotalPi0 (at !subparts i) (HyperRectangle.get_dimensions() -1) in
	      if (!max_pi0s < temp) then
	      begin
	      max_pi0s := temp; 
	      subno := i;
	      end
	  done;
	  (*check if length every edge if equals to unit*)
	  if (!max_pi0s != 1) then 
	  begin
	    print_message Debug_standard ("\nMax pi0s in list is : " ^ (string_of_int !max_pi0s) ^ " in subpart : " ^ (string_of_int !subno));
	    (*get list split subparts*)
	    let newSubpartList = sliptLongestDimensionSubpart (at !subparts !subno) in (*!subno*)
	    (*remove old subpart*)
	    subparts := (remove_at !subparts !subno);
	    (*add new subparts*)
	    subparts := !subparts@newSubpartList;
	    print_message Debug_standard ("\nList length : " ^ (string_of_int (List.length !subparts) ) );
	   end
	   else
	   begin
	    raise (Ex ("Could not split smaller and The requried Suparts/Workers larger the points in v0! Please Check Again! :P "));
	   end;
	end
	done;
	!subparts;;
	()

(*get list the max pi0 as list*)
let get_max_pi0_subpart (s : HyperRectangle.hyper_rectangle) =
	let lst = ref [] in
	for i=0 to (HyperRectangle.get_dimensions()-1) do
	lst := !lst@[NumConst.to_int(s#get_max i)];
(*	print_message Debug_standard ("\nMax pi0 : " ^ (string_of_int (NumConst.to_int(v0#get_max i)) ) );*)
	done;
	!lst;;
	()
	
(*increase 1 unit*)
let get_next_sequential_pi0_in_subpart pi0 (s : HyperRectangle.hyper_rectangle) : int array =
	(* Retrieve the current pi0 (that must have been initialized before) *)
	let current_pi0 = pi0 in
	(* Start with the first dimension *)
	let current_dimension = ref 0 in (** WARNING: should be sure that 0 is the first parameter dimension *)
	(* The current dimension is not yet the maximum *)
	let not_is_max = ref true in
(* 	 *)
	while !not_is_max do
		(* Try to increment the local dimension *)
		let current_dimension_incremented = current_pi0.(!current_dimension) + 1 in
		if current_dimension_incremented <= NumConst.to_int (s#get_max (!current_dimension)) then (
			(* Increment this dimension *)
			current_pi0.(!current_dimension) <- current_dimension_incremented;
			not_is_max := false;
		)
		(* Else: when current_dimension_incremented > Max current dimension  *)
		else ( 
			(*reset*)
			current_pi0.(!current_dimension) <- NumConst.to_int (s#get_min (!current_dimension));
			current_dimension := !current_dimension + 1;
			(* If last dimension: the end! *)
			if !current_dimension > (HyperRectangle.get_dimensions()-1) then(
				raise (Ex (" The pi0 is Max could not increase! "));
				(*not_is_max := false;*)
			)
		);
	done; (* while not is max *)
	(* Return the flag *)
	(*!more_pi0;*)
	current_pi0;;
	()


(* dynamic split subpart *)
let dynamicSplitSubpart (s : HyperRectangle.hyper_rectangle) pi0 : HyperRectangle.hyper_rectangle list =
	print_message Debug_standard ("\n entering dynamic splitting process" );
	let pi0 = get_next_sequential_pi0_in_subpart pi0 s in 
	let notFound = ref true in
	let max_d_l = ref 0 in
	let j = ref (HyperRectangle.get_dimensions()-1) in
	let lst = ref [] in
	while( !notFound (*&& (!j != -1)*) ) do
	  begin		
	    if(!j = -1) then raise (Ex (" there are only 1 pi0 left in subpart, could not split! "));
	    (*if current pi0 at max dimension j but the Min at demension j of subpart is lower, split all the done pi0 below j*)
	    (*update subpart*)
	    if ( NumConst.to_int(s#get_min (!j)) < pi0.(!j) ) then begin s#set_min (!j) (NumConst.numconst_of_int (pi0.(!j))) end;
	    max_d_l := ( (NumConst.to_int(s#get_max (!j)) - pi0.(!j) ) +1 ) ;
	    (*split subpart if the remain distance from pi0 to max dimension at leat 2 points*)
	    if( !max_d_l > 1 ) then
	      begin
		print_message Debug_standard ("\nBegin split at demension : " ^ (string_of_int (!j) ) );
		lst := split s !j;
		notFound := false ;
	      end;
	    j := (!j - 1) ;
	    (*if(!j = -1) then raise (Ex (" all demensions of subpart could not split "));*)
	    end(*end while*)
	  done;
	!lst;;
	()
	
(*dynamic split subpart 2*)
(*let splitSubpartOnFly2 (s : HyperRectangle.hyper_rectangle) pi0 : HyperRectangle.hyper_rectangle list =
	print_message Debug_standard ("\n entering dynamic splitting process" );
	
	let pi0 = get_next_sequential_pi0_in_subpart pi0 s in 
	(*check*)
	let b = checkSplitCondition pi0 s in
	if(b) then  raise (Ex (" This Subpart do not satisfy condition! "));

	let s = ref s in
	let notFound = ref true in
	(*split the larger demension first (last)*)
	let j = ref (HyperRectangle.get_dimensions()-1) in
	while(!notFound) do
	(*for i = (HyperRectangle.get_dimensions() -1) downto 0 do*)
	  begin
	    print_message Debug_standard ("\n entering while nth :" ^ (string_of_int (!j) ));
	    (*if the current point lower the dimension j then take the range between pi0 to Max dimension J for splitting*)
	    if( NumConst.to_int(!s#get_max (!j)) > pi0.(!j) ) then
	      begin
	      print_message Debug_standard ("\nBegin split at demension : " ^ (string_of_int (!j) ) );
	      let max_d_l = ref ( (NumConst.to_int(!s#get_max (!j)) - pi0.(!j) ) +1 ) in
	      (*print_message Debug_standard ("\n max_d_l :" ^ (string_of_int (!max_d_l) ));*)
	      !s#set_min (!j) (NumConst.numconst_of_int (pi0.(!j))) ;

	      (********************************************************)
	      notFound := false ;
	      (*[s1;s2];*)
	      end
	      else (*note :these cases for pi0 = Max at dimension j or raise exception for pi0 > Max*)
		begin(
		print_message Debug_standard ("\n entering pi0 = Max" );
		(*if current pi0 at max dimension j but the Min at demension j of subpart is lower, split all the done pi0 below j*)
		if ( NumConst.to_int(!s#get_min (!j)) != pi0.(!j) ) then
		  begin
		  !s#set_min (!j) (NumConst.numconst_of_int (pi0.(!j))) ;
		  print_message Debug_standard ("\n min_d_l :" ^ (string_of_int ( NumConst.to_int(!s#get_min !j)) ));
		  print_message Debug_standard ("\n max_d_l :" ^ (string_of_int ( NumConst.to_int(!s#get_max !j)) ));
		  end;
		 (*********************************************)
		)end;
	      j := (!j - 1) ;
	      if(!j = -1) then
	      begin
	      notFound := false ;
		(*raise (Ex ("Could not split smaller and The requried Suparts/Workers larger the points in v0! Please Check Again! :P "));*)
	      end;
	  end(*end while*)
	done;
	[s1;s2];;
	()*)

let receive_pull_request_and_store_constraint () =
	print_message Debug_high ("[Master] Entered function 'receive_pull_request_and_store_constraint'...");
	
	counter_master_waiting#start;
	let pull_request = receive_pull_request () in
	counter_master_waiting#stop;
	
	(** DO SOMETHING HERE **)
	
	match pull_request with
	| PullOnly source_rank ->
		print_message Debug_low ("[Master] Received PullOnly request...");
		source_rank, None

	| OutOfBound source_rank ->
		print_message Debug_low ("[Master] Received OutOfBound request...");
		(* FAIRE QUELQUE CHOSE POUR DIRE QU'UN POINT N'A PAS MARCHÉ *)
		raise (InternalError("OutOfBound not implemented."))(*;
		source_rank, None*)

	| PullAndResult (source_rank , im_result) -> 
		print_message Debug_low ("[Master] Received PullAndResult request...");
		print_message Debug_standard ("\n[Master] Received the following constraint from worker " ^ (string_of_int source_rank));
		Cartography.bc_process_im_result im_result;
		(* Return source rank *)
		source_rank, Some im_result.tile_nature;;
	()



(* hey how are yous *)

(*------------------------------------------------------------*)
(* Generic function handling the next sequential point *)
(*------------------------------------------------------------*)
let compute_next_pi0_sequentially more_pi0 limit_reached first_point tile_nature_option =
	(* Start timer *)
	counter_master_find_nextpi0#start ;

	(* Case first point *)
	if !first_point then(
		print_message Debug_low ("[Master] This is the first pi0.");
		Cartography.compute_initial_pi0 ();
		first_point := false;
	(* Other case *)
	)else(
		print_message Debug_low ("[Master] Computing next pi0 sequentially...");
		let found_pi0 , time_limit_reached = Cartography.find_next_pi0 tile_nature_option in
		(* Update the time limit *)
		limit_reached := time_limit_reached;
		(* Update the found pi0 flag *)
		more_pi0 := found_pi0;
	);
	
	(* Stop timer *)
	counter_master_find_nextpi0#stop;
	
	()
	


(*------------------------------------------------------------*)
(* Global variable for the random distributed mode *)
(*------------------------------------------------------------*)
(*** TODO: put somewhere else ***)
(* let still_random = ref true *)


(*------------------------------------------------------------*)
(* Generic function handling all cases *)
(*------------------------------------------------------------*)
let compute_next_pi0 more_pi0 limit_reached first_point tile_nature_option =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	(*match options#distribution_mode with
	(** Distributed mode: Master slave with sequential pi0 *)
	| Distributed_ms_sequential -> 
		compute_next_pi0_sequentially more_pi0 limit_reached first_point tile_nature_option
	
	(** Distributed mode: Master slave with shuffle pi0 *)
	| Distributed_ms_shuffle -> 
		compute_next_pi0_shuffle more_pi0 limit_reached first_point tile_nature_option
	
	(** Distributed mode: Master slave with random pi0 and n retries before switching to sequential mode *)
	| Distributed_ms_random nb_tries ->
		(* Test whether the algorithm is still in random mode *)
		if !still_random then(
			(* Try to find a random pi0 *)
			let successful = Cartography.random_pi0 nb_tries in
			if successful then(
				print_message Debug_low ("[Master] Computed a new random pi0.");
			)else(
				(* Switch mode ! *)
				still_random := false;
				print_message Debug_standard ("\n**************************************************");
				print_message Debug_standard ("[Master] Could not find a new random pi0 after " ^ (string_of_int nb_tries) ^ " attemps! Now switching mode from random to sequential.");
			);
		);
		(* Test whether the algorithm is now in sequential mode *)
		(*** NOTE: important to test again (instead of using 'else') in case the value was changed just above ***)
		if not !still_random then(
			print_message Debug_medium ("[Master] Now in sequential mode: computing a sequential pi0.");
			compute_next_pi0_sequentially more_pi0 limit_reached first_point tile_nature_option
		);
		print_message Debug_high ("[Master] Exiting function compute_next_pi0...");
	(**$ TODO: missing something there ***)
		
	| Distributed_unsupervised -> raise (InternalError("IMITATOR cannot be unsupervised at this point."))

	(** Normal mode *)
	| Non_distributed -> raise (InternalError("IMITATOR should be distributed at this point."))*)
	
	(** DO SOMETHING HERE **)
	()


(*------------------------------------------------------------*)
(* Functions handling shuffle mode *)
(*------------------------------------------------------------*)

(*(* Sends one point to each node (to have all nodes occupied for a while) *)
let send_one_point_to_each_node() = ()*)

(* Statically compute the shuffled array of all points *)
(*let compute_shuffled_array() =
	(* Compute the array of all points *)
	Cartography.compute_all_pi0 ();
	(* Shuffle it! *)
	Cartography.shuffle_all_pi0 ();
	()*)

	
(*------------------------------------------------------------*)
(* Some functions to implement *)
(*------------------------------------------------------------*)

(* TODO *)
let check_covered () = false
	


(*------------------------------------------------------------*)
(* The cartography algorithm implemented in the master *)
(*------------------------------------------------------------*)
let master () =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	(* Initialize counters *)
	counter_master_find_nextpi0#init;
	counter_master_waiting#init;
	
	print_message Debug_medium ("[Master] Hello world!");
	
	(* List of subparts maintained by the master *)
	let subparts = ref [] in
	
	(* Perform initialization *)
	Cartography.bc_initialize ();
	
	let more_subparts = ref true in
	let limit_reached = ref false in
	
	
	(*** THE ALGORITHM STARTS HERE ***)
	while not (check_covered ()) do
		
		(* Get the pull_request *)
(* 		let source_rank, tile_nature_option = receive_pull_request_and_store_constraint () in *)
		print_message Debug_medium ("[Master] heloooooo ");
(* 		print_message Debug_medium ("[Master] Received a pull request from worker " ^ (string_of_int source_rank) ^ ""); *)
		
		
	
	done;
	(*** THE ALGORITHM STOPS HERE ***)
	
(*	(* To differentiate between initialization of pi0 / next_point *)
	let first_point = ref true in
	
	(* For the end of the algorithm *)
	let workers_done = ref 0 in
	
	(* If random: start with random (switch to sequential later) *)
	begin
	match options#distribution_mode with
		| Distributed_ms_random _ -> still_random := true;
		| _ -> ();
	end;
	
	(* IF precompute: Compute the first point pi0 *)
	if options#precomputepi0 then(
		(*** WARNING: this may lead to an unexpected behavior if mode is shuffle ! ***)
		compute_next_pi0 more_subparts limit_reached first_point None;
(* 		Cartography.compute_initial_pi0 (); *)
	);
	
	(* IF shuffle mode: first sends one point to each node, and then statically compute the shuffled array of all points *)
	if options#distribution_mode = Distributed_ms_shuffle then(
		(* First sends one point to each node (to have all nodes occupied for a while) *)
		(*** NO!!!! just ask the slaves to do this ***)
(*		print_message Debug_standard ("[Master] Shuffle mode: sending one point to each node");
		send_one_point_to_each_node();*)
		
		(* In the meanwhile: statically compute the shuffled array of all points *)
		print_message Debug_standard ("[Master] Shuffle mode: starting to compute the shuffled array of points");
		compute_shuffled_array();
		print_message Debug_standard ("[Master] Shuffle mode: finished to compute the shuffled array of points");
	);
	(* end if shuffle *)
	
	(* Iterate on all the possible pi0 *)
	while !more_subparts && not !limit_reached do
		print_message Debug_low ("[Master] Waiting for a pull request");
		
		(* Get the pull_request *)
		let source_rank, tile_nature_option = receive_pull_request_and_store_constraint () in
		print_message Debug_medium ("[Master] Received a pull request from worker " ^ (string_of_int source_rank) ^ "");
		
		(* IF no-precompute: compute pi0 NOW *)
		if not options#precomputepi0 then(
			print_message Debug_high ("[Master] Computing pi0...");
			compute_next_pi0 more_subparts limit_reached first_point tile_nature_option;
		);
		
		(* Access the pi0 *)
		let pi0 = Cartography.get_current_pi0 () in
		
		(* If finished: say goodbye *)
		if !limit_reached || not !more_subparts then(
			send_finished source_rank;
			workers_done := !workers_done + 1;
			print_message Debug_medium( "\t[Master] - [Worker " ^ (string_of_int source_rank ) ^ "] is done");
		(* Otherwise send pi0 *)
		)else(
			print_message Debug_medium ( "[Master] Sending pi0 to [Worker " ^ (string_of_int source_rank ) ^ "]" );
			send_pi0 pi0 source_rank;
		);

		(* IF precompute: Compute the next pi0 for next time, and return flags for more pi0 and co *)
		(*** WARNING: computing the pi0 BEFORE it is asked may be stupid! It may be smarter to compute it on demand (to be compared) ***)
		if options#precomputepi0 && not !limit_reached && !more_subparts then(
			compute_next_pi0 more_subparts limit_reached first_point tile_nature_option;
		);
	done;


	(*** NOTE: we could check here (and at every further iteration) whether all integer points are already covered!!! If yes, stop. ***)
	
	print_message Debug_standard ( "[Master] Done with sending pi0; waiting for last results.");

	let size = Mpi.comm_size Mpi.comm_world in
		while !workers_done < ( size - 1) do
		print_message Debug_medium ("[Master] " ^ ( string_of_int ( size - 1 - !workers_done )) ^ " workers left" );
		let source_rank , _ = receive_pull_request_and_store_constraint () in
		print_message Debug_medium ("[Master] Received from [Worker " ^ ( string_of_int source_rank ) ^"]");
		(* Say good bye *)
		send_finished source_rank;
		workers_done := !workers_done + 1;
		print_message Debug_medium( "\t[Master] - [Worker " ^ (string_of_int source_rank ) ^ "] is done");
	done;
		
	print_message Debug_standard ("[Master] All workers done" );

	(* Process the finalization *)
	Cartography.bc_finalize ();
	
	print_message Debug_standard ("[Master] Total waiting time     : " ^ (string_of_float (counter_master_waiting#value)) ^ " s");
	print_message Debug_standard ("**************************************************");

	
	(* Process the result and return *)
	let tiles = Cartography.bc_result () in
	(* Render zones in a graphical form *)
	if options#cart then (
		Graphics.cartography (Input.get_model()) (Input.get_v0()) tiles (options#files_prefix ^ "_cart_patator")
	) else (
		print_message Debug_high "Graphical cartography not asked: graph not generated.";
	);*)
	
	()


(****************************************************************)
(**        WORKER         *)
(****************************************************************)

let init_slave rank size =
	print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] I am worker " ^ (string_of_int rank) ^ "/" ^ (string_of_int (size-1)) ^ ".");
	()


let worker() =

	(* Get the model *)
	let model = Input.get_model() in
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	(* Init counters *)
	counter_worker_waiting#init;
	counter_worker_working#init;
	
	let rank = Mpi.comm_rank Mpi.comm_world in
	let size = Mpi.comm_size Mpi.comm_world in
	init_slave rank size;
	
	let finished = ref false in
	
	begin
	match options#distribution_mode with 
		(* First start by compyting a random pi0 (while the master is computing the shuffle list) *)
		| Distributed_ms_shuffle -> 
			(*** TODO !!! TODO !!! TODO !!! ***)
			(*** NOTE: in fact, maybe NOT efficient; even for a big V0 (500,000 points), the shuffle takes only a few seconds (12s) ***)
			(*** TODO: test and compare ***)
			send_work_request ();
		(* Otherwise just ask lazily for some work *)
		| _ ->
			(* Ask for some work *)
			send_work_request ();
	end;
		
	print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] sent pull request to the master.");
	
	(* In the meanwhile: compute the initial state *)
	let init_state = Reachability.get_initial_state_or_abort model in
	
	while not !finished do
		
		counter_worker_waiting#start;
		let work = receive_work () in
		counter_worker_waiting#stop;
		
		match work with
		
		(* receive a chunk of work *)
		| Work pi0 ->
			counter_worker_working#start;

			print_message Debug_medium( "[Worker " ^ ( string_of_int rank ) ^ "] received work. Send a result." );

			(* Set the new pi0 *)
			Input.set_pi0 pi0;
			
			(* Print some messages *)
			print_message Debug_medium ("\n**************************************************");
			print_message Debug_medium ("BEHAVIORAL CARTOGRAPHY ALGORITHM: "(* ^ (string_of_int !current_iteration) ^ ""*));
			print_message Debug_standard ("\n[Worker " ^ (string_of_int rank) ^ "] Launching IM for the following pi:" (*^ (string_of_int !current_iteration)*));
			print_message Debug_standard (ModelPrinter.string_of_pi0 model pi0);
			
			(* Save debug mode *)
			let global_debug_mode = get_debug_mode() in 
			
			(* Prevent the debug messages (except in verbose modes high or total) *)
			if not (debug_mode_greater Debug_high) then
					set_debug_mode Debug_nodebug;
			
			(* Call IM *)
			let im_result , _ = Reachability.inverse_method_gen model init_state in
					
			(* Get the debug mode back *)
			set_debug_mode global_debug_mode;
			
			print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] finished a computation of IM.");
					
			(* Process the result by IM *)
			(*** TODO (cannot jus call process_im_result) ***)
			
			(* Print message *)
			print_message Debug_medium (
					"\n[Worker " ^ (string_of_int rank) ^ "] K computed by IM after "
					^ (string_of_int im_result.nb_iterations) ^ " iteration" ^ (s_of_int im_result.nb_iterations) ^ ""
					^ " in " ^ (string_of_seconds im_result.total_time) ^ ": "
					^ (string_of_int im_result.nb_states) ^ " state" ^ (s_of_int im_result.nb_states)
					^ " with "
					^ (string_of_int im_result.nb_transitions) ^ " transition" ^ (s_of_int im_result.nb_transitions) ^ " explored.");
			
			(*** TODO: handle a special case if the result is NOT valid (e.g., stopped before the end due to timeout or state limit reached) ***)
			
			send_result  im_result;
			print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] Sent a constraint ");

			counter_worker_working#stop;
			
		(* The end *)
		| Stop ->
			print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] I was just told to stop work.");
			finished := true
	done;
	
	(* Print some information *)
	let occupancy = counter_worker_working#value /. (counter_worker_working#value +. counter_worker_waiting#value) *. 100. in
	print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] I'm done.");
	print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] Total waiting time     : " ^ (string_of_float (counter_worker_waiting#value)) ^ " s");
	print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] Total working time     : " ^ (string_of_float (counter_worker_working#value)) ^ " s");
	print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] Occupancy              : " ^ (string_of_float occupancy) ^ " %");
()




(*------------------------------------------------------------*)
(* Tests *)
(*------------------------------------------------------------*)

(*implement the master*)
(*let test_gia () =


	print_message Debug_standard "--------------------Starting test !-------------------- \n"; 
	counter_master_waiting#start;
	(*************Sample Data v0************)

	HyperRectangle.set_dimensions 3;
	let v0 = new HyperRectangle.hyper_rectangle in 
	
	v0#set_min 0 (NumConst.numconst_of_int 0); 
	v0#set_max 0 (NumConst.numconst_of_int 3);
	v0#set_min 1 (NumConst.numconst_of_int 0);
	v0#set_max 1 (NumConst.numconst_of_int 3);
	v0#set_min 2 (NumConst.numconst_of_int 0);
	v0#set_max 2 (NumConst.numconst_of_int 3);
	
	(* List of subparts maintained by the master *)
	let subparts = ref [] in
	subparts := !subparts@[(v0)];

	let more_subparts = ref true in
	let limit_reached = ref false in
	print_message Debug_standard ("\nInitial list length : " ^ (string_of_int (List.length !subparts) ) );

	(*pi0*)
	(*let pi0 = [|0;2;2|] in*)
	let pi0 = [|3;1;0|] in
	
	(*let b = checkSplitCondition pi0 v0 in
	if(b) then  raise (Ex (" This Subpart do not satisfy condition! "));*)
	
	(*test split function*)
	(*split v0 2;*)
	
	(*test initialize subparts function*)
	(*subparts := (intialize_Subparts !subparts 500);*)
	
	(*test dynamicSplitSubpart*)
	(*subparts := dynamicSplitSubpart v0 pi0 ;*)

	counter_master_waiting#stop;
	print_message Debug_standard ("[Master] Total waiting time     : " ^ (string_of_float (counter_master_waiting#value)) ^ " s");
	print_message Debug_standard "\n --------------------End of test !--------------------"; 
	
()*)

;;
(*test_gia();
abort_program();*)