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
let counter_master_find_nextpi0		= new Counter.counter
let counter_worker_waiting 		= new Counter.counter
let counter_worker_working 		= new Counter.counter


(****************************************************************)
(**     MASTER      *)
(****************************************************************)

let first (a,b) = a
let second (a,b) = b

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
let intialize_Subparts (v0 : HyperRectangle.hyper_rectangle) (n : int) =
	let subparts = ref [v0] in
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
(*let get_max_pi0_subpart (s : HyperRectangle.hyper_rectangle) =
	let lst = ref [] in
	for i=0 to (HyperRectangle.get_dimensions()-1) do
	lst := !lst@[NumConst.to_int(s#get_max i)];
(*	print_message Debug_standard ("\nMax pi0 : " ^ (string_of_int (NumConst.to_int(v0#get_max i)) ) );*)
	done;
	!lst;;
	()*)

(*get number of points in subpart*)
let get_points_in_subpart (s : HyperRectangle.hyper_rectangle)=
	let total = ref 1 in
	for i=0 to (HyperRectangle.get_dimensions()-1) do
	total := !total * (NumConst.to_int(s#get_max i)+1);
	done;
	!total;;
	()

(*compute the how many points was done in subpart*)
let done_points_in_subpart (s : HyperRectangle.hyper_rectangle) (arr : AbstractModel.pi0) =
	let sum = ref 0 in
	let tail = ref 1 in
	for i= (HyperRectangle.get_dimensions()-1) downto 0 do
	  for j=i-1 downto 0 do
	    tail := !tail*(NumConst.to_int(s#get_max j)+1) ;
	  done;
	  sum := !sum + ( (NumConst.to_int (arr#get_value(i))) * !tail );
	  tail := 1;
	done;
	!sum +1;;
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
	
	

let receive_pull_request () =
	print_message Debug_high ("[Master] Entered function 'receive_pull_request_and_store_constraint'...");
	
	counter_master_waiting#start;
	let pull_request = receive_pull_request () in
	counter_master_waiting#stop;
	
	(** DO SOMETHING HERE **)
	
	match pull_request with
	| PullOnly source_rank ->
		print_message Debug_low ("[Master] Received PullOnly request...");
		(source_rank, None , None)

	| OutOfBound source_rank ->
		print_message Debug_low ("[Master] Received OutOfBound request...");
		(* FAIRE QUELQUE CHOSE POUR DIRE QU'UN POINT N'A PAS MARCHÉ *)
		raise (InternalError("OutOfBound not implemented."))(*;
		source_rank, None*)
		
	(*Hoang Gia new messages*)
	| Tile (source_rank , im_result) ->
		print_message Debug_low ("[Master] Received Tile ...");
		print_message Debug_standard ("\n[Master] Received the following constraint from worker " ^ (string_of_int source_rank));
		Cartography.bc_process_im_result im_result; (*send to all workers*)
		(* Return source rank *)
		(source_rank, Some im_result , None )
	
	| Pi0 (source_rank , pi0) ->
	      (*print_message Debug_low ("[Master] Received pi0 ...");
	      print_message Debug_standard ("\n[Master] Received the following pi0 from worker " ^ (string_of_int source_rank));*)
	      (source_rank , None , Some pi0)
	      
	| _ ->  raise (InternalError("have not implemented.")) 
	()



(* hey how are yous *)


(* pval to array *)
let pval2array pval =
	let arr = Array.make (PVal.get_dimensions()) 0 in
	(*print_message Debug_standard ("\nPVal dimensions : " ^ (string_of_int (PVal.get_dimensions()) ) );*)
	for i = 0 to (PVal.get_dimensions()-1) do
	  arr.(i) <- (NumConst.to_int (pval#get_value i));
	done;
	arr;;
	()

	
(*------------------------------------------------------------*)
(* Some functions to implement *)
(*------------------------------------------------------------*)


(*------------------------------------------------------------*)
(* The cartography algorithm implemented in the master *)
(*------------------------------------------------------------*)
(*Hoang Gia master implementation*)
let master () =
	(* Retrieve the input options *)
	(*let options = Input.get_options () in*)
	
	let check_covered = ref false in
	
	(* Get the model *)
	let model = Input.get_model() in
	(* Get the v0 *)
	let v0 = Input.get_v0() in
	(* number of subpart want to split *)
	let np = -1 in
	
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
	
	
	
	(*initialize list of subpart*)
	subparts := intialize_Subparts v0 np;
	
	(*print_message Debug_standard ("[Master] Here!!!!!!!!!!");*)
	
	(* create index(worker,supart) *)
	let index = ref [] in
	
	(* current pi0 of workers*)
	let current_Pi0 = ref [] in
	
	(* stopSplitting flag *)
	let stopSplitting = ref false in
	
	(*** THE ALGORITHM STARTS HERE ***)
	while (not !check_covered) do

		(*count number of subparts were sent*)
		let count = ref 0  in
		(* Get the pull_request *)
		let source_rank, tile_nature_option, pi0 = (receive_pull_request()) in
		print_message Debug_medium ("[Master] heloooooo ");
		let msg = ref (tile_nature_option, pi0) in
		match !msg with 
		(*Pull Tag*)
		(None , None) -> print_message Debug_medium ("[Master] Received a pull request from worker " ^ (string_of_int source_rank) ^ "");
				 (* check to delete if the worker comeback *)
				 if(List.mem_assoc source_rank !index) then
				  begin
				    index := List.remove_assoc source_rank !index;
				  end;
				  
				 (*case : semd all of subpart in the list*)
				 if not (!count = (List.length !subparts)) then 
				  begin
				    (*send new subpart *)
				    send_subpart (at (!subparts) (!count)) source_rank;
				    print_message Debug_medium ("[Master] Sent Subpart to worker " ^ (string_of_int source_rank) ^ "");
				    (*add into index*)
				    index := !index@[( source_rank, (List.hd !subparts) )];
				    count := !count +1;
				  end
				 (*have not any subpart in list -> splitting*)
				 else
				  begin
				    if(not !stopSplitting) then
				      begin
					let remain = ref 0 in
					let markedWorker = ref 0 in
					(*let marked = ref 0 in*)
					(*find the most incomplete points subpart*)
					for i=0 to (List.length (!index))-1 do
					    (*get worker in index*)
					    let worker = first (List.nth !index i) in
					    (*compute the remain points by get number of points in subpart - the done points*)
					    let size = get_points_in_subpart (List.assoc worker !index) in
					    let done_points = done_points_in_subpart (List.assoc worker !index) (List.assoc worker !current_Pi0) in
					    let temp = size - done_points in
					    if (temp > !remain) then
					      begin
						remain := temp;
						(*marked := i;*)
						markedWorker := worker;
					      end;
					done; (*for*)
					 (*check if this largest subpart is splittable*)
					 if (!remain > 2) then 
					  begin
					    (*dynamic splitting here*)
					    (*stop the worker for awhile*)
					    send_finished !markedWorker;
					    (*get splittable largest subpart*)
					    let subpart = List.assoc !markedWorker !index in
					    let current_pi0 = List.assoc !markedWorker !current_Pi0 in
					    (**)
					    let current_pi0_arr = pval2array current_pi0 in
					    let newSubparts = dynamicSplitSubpart subpart current_pi0_arr in
					    (*send to the old worker the first subpart*)
					    send_subpart (at newSubparts 0) (!markedWorker);
					    (*send to the new worker the splitSubpart*)
					    send_subpart (at newSubparts 1) (source_rank);
					    (*then add into the index the new worker with new splitSubpart *)
					    index := !index@[(source_rank, (at newSubparts 1))];
					    (*worker := first (List.nth !index !marked);*)
					    (*print_message Debug_medium ("[Master] heloooooo ");*)
					  end
					 (*the points left in subpart less than 2*)
					 else
					  begin
					    (*send terminate to the worker*)
					    send_terminate source_rank;
					    (*set the stopSplitting = true*)
					    (*stopSplitting := true;*)
					  end;
					end (* stopSplitting *)
				    (*case stop splitting = true -> send terminate to workers which send pull msg*)
				    else
				      begin
					  (*send terminate to the worker*)
					  send_terminate source_rank;
				      end;
				    end;
				  
		
		
		(*Tile Tag*)
		|(Some x, None) -> print_message Debug_medium ("[Master] Received a tile from worker " ^ (string_of_int source_rank) ^ "");
				   (*check duplicated tile*)
				   (*let tiles = Cartography.bc_result () in*)
				   if(Cartography.bc_process_im_result x) then
				    begin
				      (*receive tile then send to the other workers to update*)
				      for i = 0 to (List.length !index)-1 do
					if ( (first (List.nth !index i)) != source_rank ) then
					  begin
					    send_tile x (first (List.nth !index i));
					  end;
				      done
				    end;
		
		
		
		(*Pi0 Tag*)
		|(None, Some y) -> print_message Debug_medium ("[Master] Received a pi0 from worker " ^ (string_of_int source_rank) ^ "");
				   (*update the current point of worker*)
				   if(List.mem_assoc source_rank !current_Pi0) then
				    begin
				      current_Pi0 := List.remove_assoc source_rank !current_Pi0;
				    end
				   else 
				    begin
				      current_Pi0 := !current_Pi0@[( source_rank, y)];
				    end
				    
		(*0ther cases*)
		|_ -> raise (InternalError("have not implemented."));
		
		(**)
		if(!index = []) then begin check_covered := true end;
		
	done;;
	()


(****************************************************************)
(**        WORKER         *)
(****************************************************************)

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



let init_slave rank size =
	print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] I am worker " ^ (string_of_int rank) ^ "/" ^ (string_of_int (size-1)) ^ ".");
	()


let worker() =
	(* Perform initialization *)
	Cartography.bc_initialize ();
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
	
	(* Access the pi0 *)
(*	let pi0 = Cartography.get_current_pi0 () in
	v0 = getsubpqrt in
	Input.set_v0 v0;*)
		
	(* In the meanwhile: compute the initial state *)
	let init_state = Reachability.get_initial_state_or_abort model in
	

	
	while not !finished do
	
		send_work_request ();
		print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] sent pull request to the master.");
		
		counter_worker_waiting#start;
		let work = receive_work () in
		counter_worker_waiting#stop;
		
		match work with
		
		| Subpart subpart -> 
			print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] received subpart from Master.");
			
			(* To differentiate between initialization of pi0 / next_point *)
			let first_point = ref true in
			
			let more_pi0 = ref false in
			
			(*initialize subpart*)
			Input.set_v0 subpart;
			(*print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] setted v0.");*)
			
			(*initial pi0*)
			Cartography.compute_initial_pi0();
			print_message Debug_medium ("Worker debug!!! ");
			
(*			while !more_pi0 do
			    
			    let pi0 = Cartography.get_current_pi0 () in
			    (* Call IM *)
			    let im_result , _ = Reachability.inverse_method_gen model init_state in
			    
			    
			    Cartography.compute_next_sequential_pi0
			done;*)
			
			finished := true;

			
		| Terminate -> print_message Debug_medium (" Terminate ");
			print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] I was just told to terminate work.");
			finished := true
		
		
		| Tile tile -> print_message Debug_medium (" Tile ");
		
		| Stop ->
			print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] I was just told to stop work.");
			finished := true
			
		| _ -> raise (InternalError("have not implemented."))
		      
		
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
  master();
  worker();*)
    

(*	print_message Debug_standard "--------------------Starting test !-------------------- \n"; 
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
	
(*	v0#set_min 0 (NumConst.numconst_of_int 0); 
	v0#set_max 0 (NumConst.numconst_of_int 4);
	v0#set_min 1 (NumConst.numconst_of_int 0);
	v0#set_max 1 (NumConst.numconst_of_int 4);*)
	
	(* List of subparts maintained by the master *)
(*	let subparts = ref [] in
	subparts := !subparts@[(v0)];

	print_message Debug_standard ("\nInitial list length : " ^ (string_of_int (List.length !subparts) ) );*)

	(*pi0*)
	(*let pi0 = [|0;2;2|] in*)
	let pi0 = [|4;4|] in
	
	(*let b = checkSplitCondition pi0 v0 in
	if(b) then  raise (Ex (" This Subpart do not satisfy condition! "));*)
	
	(*test split function*)
	(*split v0 2;*)
	
	(*test initialize subparts function*)
	(*subparts := (intialize_Subparts !subparts 500);*)
	
	(*test dynamicSplitSubpart*)
	(*subparts := dynamicSplitSubpart v0 pi0 ;*)
	
	(*check done points in subpart*)
	(*let done_points = done_points_in_subpart v0 pi0 in
	print_message Debug_standard ("\n done points : " ^ (string_of_int (done_points) ) );*)
	
	(*test pval to array*)
	(*create an instance pval*)
	(*PVal.set_dimensions 2;
	let pval = new PVal.pval in
	pval#set_value 0 (NumConst.numconst_of_int 2);
	pval#set_value 1 (NumConst.numconst_of_int 2);
	print_message Debug_standard ("\n pval 0 : " ^ (string_of_int (NumConst.to_int (pval#get_value 0)) ) );
	print_message Debug_standard ("\n pval 1 : " ^ (string_of_int (NumConst.to_int (pval#get_value 1)) ) );
	let arr = pval2array pval in
	print_message Debug_standard ("\n array 0 : " ^ (string_of_int  (arr.(0)) ) );
	print_message Debug_standard ("\n array 1 : " ^ (string_of_int  (arr.(1)) ) );*)
	
	
	counter_master_waiting#stop;
	print_message Debug_standard ("[Master] Total waiting time     : " ^ (string_of_float (counter_master_waiting#value)) ^ " s");
	print_message Debug_standard "\n --------------------End of test !--------------------"; *)
	
(*()*)

(*;;*)
(*test_gia();
abort_program();*)
