(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre, Camille Coti, Hoang Gia Nguyen
 * 
 * Created:       2014/09/05
 * Last modified: 2015/04/20
 *
 ****************************************************************)

 
  !!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!
 WARNING !!! THIS FILE IS NOW UNPLUGGED FROM THE IMITATOR SOURCE CODE (as of 2016/03/10)
 This paragraph should raise a compiling error (syntax error) if by any chance this file was linked from another file.
  !!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!

(****************************************************************)
(* External modules *)
(****************************************************************)
open Mpi


(****************************************************************)
(* Internal modules *)
(****************************************************************)
open OCamlUtilities
open Exceptions
open ImitatorUtilities
open Options
open DistributedUtilities

exception KillIM;;

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Counters *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(*------------------------------------------------------------*)
(* Master *)
(*------------------------------------------------------------*)
let counter_master_total 			= new Counter.counter
let counter_master_processing 			= new Counter.counter
let counter_master_waiting 			= new Counter.counter
let counter_master_split			= new Counter.counter


(*------------------------------------------------------------*)
(* Workers *)
(*------------------------------------------------------------*)

let counter_worker_total 			= new Counter.counter
let counter_worker_waiting 			= new Counter.counter
let counter_worker_IM	 			= new Counter.counter
let counter_worker_find_next_pi0	= new Counter.counter





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
	
(*remove element at*)	
let rec remove_at_lst lst n =
	match lst with
	| [] -> [];
	| h :: t -> if n = 0 then t else h :: remove_at_lst t (n-1);;
    
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
	
(*initial Subparts function: put in the Subpart list with number of the Workers
	return the new split subpart list*)
let intialize_Subparts (v0 : HyperRectangle.hyper_rectangle) (n : int) =
	let subparts = ref [v0] in
	for l = 0 to n - 1 do 
	begin
	
 (* Find the largest subpart to split *)
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
	    print_message Verbose_medium ("\nMax pi0s in list is : " ^ (string_of_int !max_pi0s) ^ " in subpart : " ^ (string_of_int !subno));
	    (*get list split subparts*)
	    let newSubpartList = sliptLongestDimensionSubpart (at !subparts !subno) in (*!subno*)
	    (*remove old subpart*)
	    subparts := (remove_at !subparts !subno);
	    (*add new subparts*)
	    subparts := !subparts@newSubpartList;
	    print_message Verbose_medium ("\nList length : " ^ (string_of_int (List.length !subparts) ) );
	   end
	   else
	   begin
	    raise (InternalError ("Could not split smaller and The requried Suparts/Workers larger the points in v0! Please Check Again! :P "));
	   end;
	end
	done;
	!subparts;;
	()


(*get number of points in subpart*)
let get_points_in_subpart (s : HyperRectangle.hyper_rectangle)=
	let total = ref 1 in
	for i=0 to (HyperRectangle.get_dimensions()-1) do
	total := !total * (NumConst.to_int(s#get_max i) - NumConst.to_int(s#get_min i) + 1);
	done;
	!total;;
	()
	
(*compute the how many points was done in subpart*)
let done_points_in_subpart (s : HyperRectangle.hyper_rectangle) (arr : AbstractModel.pi0) =
	let sum = ref 0 in
	let tail = ref 1 in
	for i= 1 to (HyperRectangle.get_dimensions()-1) do
	  for j=0 to i-1 do
	    tail := !tail*(NumConst.to_int(s#get_max j) - NumConst.to_int(s#get_min j) +1) ;
	  done;
	  sum := !sum + ( (NumConst.to_int (arr#get_value(i))) - (NumConst.to_int(s#get_min i))) * !tail;
	  tail := 1;
	done;
	!sum + (NumConst.to_int (arr#get_value(0))) - (NumConst.to_int(s#get_min 0)) + 1;;
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
				raise (InternalError (" The pi0 is Max could not increase! "));
				(*not_is_max := false;*)
			)
		);
	done; (* while not is max *)
	(* Return the flag *)
	(*!more_pi0;*)
	current_pi0;;
	()


(* dynamic split subpart *)
let dynamicSplitSubpart (s : HyperRectangle.hyper_rectangle) pi0 n : HyperRectangle.hyper_rectangle list =
	print_message Verbose_medium ("\n entering dynamic splitting process" );
	(*let pi0 = get_next_sequential_pi0_in_subpart pi0 s in *)
	let notFound = ref true in
	let max_d_l = ref 0 in
	let j = ref (HyperRectangle.get_dimensions()-1) in
	let lst = ref [] in
	(*print_message Verbose_medium ("\n bug!!!!!!!!!!0" );*)
	while( !notFound (*&& (!j != -1)*) ) do
	  begin
	 (* print_message Verbose_medium ("\n bug!!!!!!!!!!1" );*)
	   (* if(!j = -1) then  print_message Verbose_medium ("\n all demensions of subpart could not split" ); raise (InternalError (" there are only 1 pi0 left in subpart, could not split! "));*)
	    (*if current pi0 at max dimension j but the Min at demension j of subpart is lower, split all the done pi0 below j*)
	    (*update subpart*)
	   (* print_message Verbose_medium ("\n bug!!!!!!!!!!2" );*)
	    if ( NumConst.to_int(s#get_min (!j)) < pi0.(!j) ) then begin s#set_min (!j) (NumConst.numconst_of_int (pi0.(!j))) end;
	   (* print_message Verbose_medium ("\n bug!!!!!!!!!!3" );*)
	    max_d_l := ( (NumConst.to_int(s#get_max (!j)) - pi0.(!j) ) +1 ) ;
	    (*print_message Verbose_medium ("\n bug!!!!!!!!!!4" );*)
	    (*split subpart if the remain distance from pi0 to max dimension at leat 2 points*)
	    if( !max_d_l > 1 ) then
	      begin
	      (*print_message Verbose_medium ("\n bug!!!!!!!!!!5" );*)
		print_message Verbose_medium ("\nBegin split at demension : " ^ (string_of_int (!j) ) );
		lst := split2 s !j n;
		notFound := false ;
	      end;
	    j := (!j - 1) ;
	   (* if(!j = -1) then
	      begin
		print_message Verbose_medium ("\n all demensions of subpart could not split" );
		raise (InternalError (" all demensions of subpart could not split "));
	      end;*)
	    
	    end(*end while*)
	  done;
	  (*if(!j = -1) then
	    begin
	      
	      print_message Verbose_medium ("\n all demensions of subpart could not split" );
	      raise (InternalError (" all demensions of subpart could not split "));
	    end;*)
	    print_message Verbose_medium ("\n splitting list : " ^ (string_of_int (List.length !lst) ) );
	!lst;;
	()


(* hey how are yous *)
	(*** what is that ?????? ***)

(* pval to array *)
let pval2array pval =
	let arr = Array.make (PVal.get_dimensions()) 0 in
	(*print_message Verbose_standard ("\nPVal dimensions : " ^ (string_of_int (PVal.get_dimensions()) ) );*)
	for i = 0 to (PVal.get_dimensions()-1) do
	  arr.(i) <- (NumConst.to_int (pval#get_value i));
	done;
	arr;;
	()
	
let pi0InV0 pi0 v0 =
	let c = ref true in
	(*for i = 0 to (HyperRectangle.get_dimensions()-1) do*)
	let i = ref 0 in
	while( (!i != HyperRectangle.get_dimensions()) && !c ) do
	begin
	  if( ((pi0#get_value !i) > (v0#get_max !i)) || ((pi0#get_value !i) < (v0#get_min !i)) ) then
	    begin
	      c := false;
	    end;
	    i := !i + 1;
	end
	done;
	!c;;
	

(*------------------------------------------------------------*)
(* The cartography algorithm implemented in the master *)
(*------------------------------------------------------------*)
(*Hoang Gia master implementation*)
let master () =
	
	 (*Retrieve the input options *)
(* 	let options = Input.get_options () in *)
	
	let check_covered = ref false in
	
	(* Get the model *)
(* 	let model = Input.get_model() in *)
	(* Get the v0 *)
	let v0 = Input.get_v0() in
	
	(* Initialize counters *)
(* 	counter_master_find_nextpi0#init; *)
	counter_master_total#init;
	counter_master_processing#init;
	counter_master_waiting#init;
	counter_master_split#init;
		
	(* Start total counter *)
	counter_master_total#start;
	
	print_message Verbose_medium ("[Master] Hello world!");
	
	(* Perform initialization *)
	counter_master_processing#start;
	Cartography.bc_initialize ();
	counter_master_processing#stop;
	
	(* List of subparts maintained by the master *)
	let subparts = ref [] in
	
(* 	let more_subparts = ref true in *)
(* 	let limit_reached = ref false in *)
	

	(* create index(worker,supart) *)
	let index = ref [] in
	(* current pi0 of workers*)
(* 	let current_Pi0 = ref [] in *)
	(* stopSplitting flag *)
(* 	let stopSplitting = ref false in *)
	(*waitting *)
	let waittingList = ref [] in
	
	
	(******************Adjustable values********************)
	(* number of subpart want to initialize in the first time *)

	(*** WARNING: why "-2" ??? ***)
	
	let np = (DistributedUtilities.get_nb_nodes ()) -2 in
	(*depend on size of model*)
(* 	let dynamicSplittingMode = ref true in *)
	(*******************************************************)
	
	(*initialize list of subpart*)
	counter_master_split#start;
	subparts := intialize_Subparts v0 np;
	counter_master_split#stop;
	
	let tilebuffer = ref [] in
	
	let wastedTiles = ref 0 in
	
	let pi0buffer = ref [] in

	
	(*** THE ALGORITHM STARTS HERE ***)
	while (not !check_covered) do
	  begin
		
		(* Get the pull_request *)
		(*let source_rank, tile_nature_option, pi0 = (receive_pull_request_subpart()) in*)
		print_message Verbose_medium ("[Master] heloooooo ");
		
		counter_master_waiting#start;
		let pull_request = (receive_pull_request ()) in
		counter_master_waiting#stop;
		
		(*send_terminate source_rank;*)
		match pull_request with 
		(*Pull Tag*)
		| PullOnly source_rank ->
				 print_message Verbose_medium ("[Master] Received a pull request from worker " ^ (string_of_int source_rank) ^ "");
				 (* check to delete if the worker comeback *)
				 if(List.mem_assoc source_rank !index) then
				  begin
				    index := List.remove_assoc source_rank !index;
				  end;
				  
				  if (!subparts != [])  then 
				  begin
				    (*send new subpart *)
				    send_subpart (List.hd !subparts) source_rank;
				    print_message Verbose_medium ("[Master] Sent Subpart to worker " ^ (string_of_int source_rank) ^ "");
				    (*add into index*)
				    index := !index@[( source_rank, (List.hd !subparts) )];
				    subparts := List.tl !subparts;
				  end
				 (*have not any subpart in list -> splitting* , if could not split -> terminate*)
				 else
				  begin

					waittingList := !waittingList@[source_rank];
					print_message Verbose_high ("[Master] Worker " ^ (string_of_int source_rank) ^ " go to waittingList");
					print_message Verbose_high ("[Master] Waiting list size : " ^ (string_of_int (List.length !waittingList)) );

				  end;
				  print_message Verbose_medium ("[Master] Received a pull request from worker " ^ (string_of_int source_rank) ^ "; end.");
		(*Tile Tag*)
		| Tile (source_rank , tile) ->
				   print_message Verbose_medium ("[Master] Received a tile from worker " ^ (string_of_int source_rank) ^ "");
				   (*check duplicated tile*)
				   	counter_master_processing#start;
					let constraint_added = Cartography.bc_process_im_result tile in
				   	counter_master_processing#stop;

				   if constraint_added then
				    begin
				      (*receive tile then send to the other workers to update*)
				      for i = 0 to (List.length !index)-1 do
					if ( (first (List.nth !index i)) != source_rank ) then
					  begin
					   (* send_tile tile (first (List.nth !index i));*)
					   tilebuffer := !tilebuffer@[(first (List.nth !index i)), tile];
					  end;
				      done
				    end
				    (* Otherwise: the constraint was not added because it existed already *)
				    else 
				    begin
				      wastedTiles := !wastedTiles + 1;
				    end;
				    print_message Verbose_medium ("[Master] Received a tile from worker " ^ (string_of_int source_rank) ^ "");

		(*Pi0 Tag*)
		| Pi0 (source_rank , pi0) -> 
				print_message Verbose_medium ("[Master] Received a pi0 from worker " ^ (string_of_int source_rank) ^ "");
				(*Update tiles*)
				(*while(List.mem_assoc source_rank !tilebuffer) do
				  begin
				    send_tile (List.assoc source_rank !tilebuffer) source_rank;
				    tilebuffer := (List.remove_assoc source_rank !tilebuffer);
				    print_message Verbose_medium ("[Master] send a tile to worker " ^ (string_of_int source_rank) ^ "");
				  end
				done;*)
				
				(*Update pi0s*)
				if(List.mem_assoc source_rank !pi0buffer) then
				 begin
				   pi0buffer := List.remove_assoc source_rank !pi0buffer;
				 end;
				pi0buffer := !pi0buffer@[( source_rank, pi0 )];  
				print_message Verbose_medium ("[Master] pi0s list : " ^ (string_of_int (List.length !pi0buffer) ) ^ "");
				
				
				(*Splitting*)
				(*let found_pi0 = ref false in
				Cartography.test_pi0_uncovered pi0 found_pi0 ;*)
				     if( not (!waittingList = []) (*&& !found_pi0*)) then
				      begin
					print_message Verbose_medium ("[Master] waiting List : " ^ (string_of_int (List.length !waittingList) ) ^ "");
					let s = List.assoc source_rank !index in
					
					(*compute the remain points int this subpart*)
					
					counter_master_split#start;
					let max_size = get_points_in_subpart s in
					let done_points = done_points_in_subpart s pi0 in
					counter_master_split#stop;

					let remain_points = max_size - done_points in

					print_message Verbose_medium ("[Master] max_size " ^ (string_of_int max_size) ^ "");
					print_message Verbose_medium ("[Master] done_points " ^ (string_of_int done_points) ^ "");
					
					
					 print_message Verbose_medium ("[Master] Splitting ....... ");
					if(remain_points > 1 (*(List.length !waittingList)*)) 
					  then
					  begin
					    print_message Verbose_medium ("[Master] Splitting ....... ");
					    (*if splitable, remove it in the index*)
					    index := (List.remove_assoc source_rank !index);
					    
					    let pi0arr = pval2array pi0 in
					    
						counter_master_split#start;
					    let newSubparts = dynamicSplitSubpart s pi0arr ((List.length !waittingList)+1)in
						counter_master_split#stop;
						
						(*send back to this worker*)
					    let subpart1 = List.hd newSubparts in
					    send_subpart subpart1 source_rank;
					    print_message Verbose_medium ("[Master] sent split subpart 1....... ");
					    
					     index := !index@[source_rank, subpart1];
					    
					    let subpart2 = (at newSubparts 1) in
					    let size = get_points_in_subpart subpart2 in
						let n =
						(*** WARNING: why -1 ??? ***)
					    if size >= (List.length !waittingList) then (List.length !waittingList) - 1
						(*** WARNING: why -1 ??? ***)
					    else size - 1
					     in
					    
					    let newSubparts2 = intialize_Subparts subpart2 n in
					    for i = 0 to ( (List.length newSubparts2) -1) do
						let w = List.hd !waittingList in
						send_subpart (at newSubparts2 i) w;
						index := !index@[w, (at newSubparts2 i)];
(* 						print_message Verbose_medium ("[Master] sent split subpart 2gdfgdfgf....... "); *)
						waittingList := remove_at_lst !waittingList 0;
(* 						print_message Verbose_medium ("[Master] sent split subpart 2gdfgdfgf.......1 "); *)
					    done;
					    
(* 					    print_message Verbose_medium ("[Master] sent split subpart 2gdfgdfgf.......2 "); *)
					    
					  end
				     end;
				     send_continue source_rank;
				     
				     print_message Verbose_medium ("[Master] Received a pi0 from worker " ^ (string_of_int source_rank) ^ "");
		
		| UpdateRequest source_rank ->
				print_message Verbose_medium ("[Master] Received UpdateRequest  ");
				
				(* Update tiles *)
				while(List.mem_assoc source_rank !tilebuffer) do
				  begin
				    send_tileupdate (List.assoc source_rank !tilebuffer) source_rank;
				    tilebuffer := (List.remove_assoc source_rank !tilebuffer);
				    print_message Verbose_medium ("[Master] send a tile to worker " ^ (string_of_int source_rank) ^ "");
				  end
				done;
				
				(*let uncovered = ref false in
				let currentPi0 = List.assoc source_rank !pi0buffer in
				Cartography.test_pi0_uncovered currentPi0 uncovered ;
				if(not !uncovered) then
				begin
				  print_message Verbose_medium ("[Master] send_terminate  ");
				  send_terminate source_rank ; 
				end;*)
				
				send_continue source_rank;

		(*0ther cases*)
		|_ -> raise (InternalError("not implemented."));
	  end;
	
	(**)
	if(!index = []) 
	  then 
	  begin 
	    check_covered := true;
		
	    for i = 1 to (DistributedUtilities.get_nb_nodes ())-1 do
		send_terminate i;
	    done
	  end;
	
	(*stopSplitting := true;*)
	done; (* END WHILE *)
	
	
	
	
	print_message Verbose_standard ("[Master] All workers done" );

	(* Process the finalization *)
	counter_master_processing#start;
	Cartography.bc_finalize ();
	counter_master_processing#stop;
	
	(* Stop global counter *)
	counter_master_total#stop;

	(* Print some information *)
	let occupancy = (counter_master_total#value -. counter_master_waiting#value ) /. (counter_master_total#value) *. 100. in

	print_message Verbose_standard ("[Master] Splitting time         : " ^ (string_of_float (counter_master_split#value)) ^ " s");
	print_message Verbose_standard ("[Master] Processing time        : " ^ (string_of_float (counter_master_processing#value)) ^ " s");
	print_message Verbose_standard ("[Master] Waiting time           : " ^ (string_of_float (counter_master_waiting#value)) ^ " s");
	print_message Verbose_standard ("[Master] Occupancy              : " ^ (string_of_float occupancy) ^ " %");
	print_message Verbose_standard ("[Master] wasted tiles           : " ^ (string_of_int !wastedTiles) ^ "");
	print_message Verbose_standard ("**************************************************");
	
	(* Generate the graphics *)
	Cartography.output_graphical_cartography (Some "_cart_patator");

	(* The end *)
	()



(****************************************************************)
(**        WORKER         *)
(****************************************************************)



(*** 
	New function: 
		1) check that buffer not empty
		2) if empty --> exit (nothing)
		3) if not empty, check what tag
		4) if tag = Stop then raise exception
***)
let check_stop_order () =
	
	send_update_request();
					
	print_message Verbose_medium (" send Update Request to master ");
	
	let killIM = ref false in
	
	let receivedContinue = ref false in
					
	while (not !receivedContinue) do
				
		let check = receive_work () in
		match check with
						
		(*** TODO: how can the worker receive a tile from the master ??? ***)
		| TileUpdate tile -> 		(*print_message Verbose_medium ("[Worker " (*^ (string_of_int rank) ^*) "] received Tile from Master.");*)
(* 					let b = Cartography.bc_process_im_result tile in *)
					print_message Verbose_medium ("[Worker " ^ (*(string_of_int rank) ^*) "] received Tile from Master.");
					
		(*| Terminate -> 		print_message Verbose_medium ("[Some Worker] received Terminate from Master.");
					(*raise KillIM;*)
					killIM := true;*)
						
		| Continue ->  		print_message Verbose_medium ("[Worker " ^ (*(string_of_int rank) ^*) "] received continue tag from Master.");
					let uncovered = ref false in
					let currentPi0 = (*List.assoc source_rank !pi0buffer*) Cartography.get_current_pi0() in
					Cartography.test_pi0_uncovered currentPi0 uncovered ;
					if(not !uncovered) then
					begin
					  (*print_message Verbose_medium ("[Master] send_terminate  ");
					  send_terminate source_rank ;*) 
					  killIM := true;
					end;
		      
					receivedContinue := true;	
					print_message Verbose_medium ("[Worker " ^ (*(string_of_int rank) ^*) "] received Tile from Master.");
									
		| _ -> 			print_message Verbose_medium ("error!!! receive tile at worker side." ^ (*(string_of_int rank) ^*) " ");
					raise (InternalError("error!!! receive tile at worker side."));
						
	done; (* end while *)
	
	if !killIM then raise KillIM;;


	
(*------------------------------------------------------------*)
(* Generic function handling the next sequential point *)
(*------------------------------------------------------------*)
let compute_next_pi0_sequentially more_pi0 limit_reached first_point tile_nature_option =
	(* Start timer *)
	counter_worker_find_next_pi0#start ;

	let rank = get_rank () in

	print_message Verbose_high ("[Worker " ^ (string_of_int rank) ^ "] Entering compute_next_pi0_sequentially.");
	
	(* Case first point *)
	if !first_point then(
		print_message Verbose_low ("[Worker " ^ (string_of_int rank) ^ "] This is the first pi0.");
		Cartography.compute_initial_pi0 ();
		first_point := false;
		
	(* Other case *)
	)else(
		print_message Verbose_low ("[Worker " ^ (string_of_int rank) ^ "] Computing next pi0 sequentially...");
		
		let found_pi0 , time_limit_reached = Cartography.find_next_pi0 tile_nature_option in
		
		(* Update the time limit *)
		limit_reached := time_limit_reached;
		(* Update the found pi0 flag *)
		more_pi0 := found_pi0;
		
	);
	
	(* Stop timer *)
	counter_worker_find_next_pi0#stop;
	(* The end *)
	print_message Verbose_high ("[Worker " ^ (string_of_int rank) ^ "] Exiting compute_next_pi0_sequentially.");
	()


let worker() =
	(* Get the model *)
	let model = Input.get_model() in
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	(* Init counters *)
	counter_worker_total#init;
	counter_worker_waiting#init;
	counter_worker_IM#init;
	counter_worker_find_next_pi0#init;
	
	(* Start global counter *)
	counter_worker_total#start;
	
	(* Assign the termination function to the inverse method *)
	if options#distributedKillIM then(
		Reachability.set_patator_termination_function check_stop_order;
	);
	
	let rank = get_rank () in
	let size = get_nb_nodes () in

	print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] I am worker " ^ (string_of_int rank) ^ "/" ^ (string_of_int (size-1)) ^ ".");
	
	let finished = ref false in
	
	(* In the meanwhile: compute the initial state *)
	let init_state = Reachability.get_initial_state_or_abort model in
	
	(* Initialize the cartography *)
	Cartography.bc_initialize ();
	
	(*let main_loop () = *)
	while (not !finished) do
		send_work_request ();
		print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] sent pull request to the master.");
		counter_worker_waiting#start;

		let work = receive_work() in
		counter_worker_waiting#stop;
		match work with
		| Subpart subpart -> 
			print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] received subpart from Master.");
			
			
			(* To differentiate between initialization of pi0 / next_point *)
			let first_point = ref true in
			
			let more_pi0 = ref true in
			
			let limit_reached = ref false in
			
			(*initialize subpart*)
			Input.set_v0 subpart;
			
			(* Perform initialization *)
			Cartography.bc_initialize_subpart ();
			
			if verbose_mode_greater Verbose_medium then(
				print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] set v0:");
				print_message Verbose_medium (ModelPrinter.string_of_v0 model subpart);
			);
			
			(*initial pi0*)
			Cartography.compute_initial_pi0();
			
			print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] Initial pi0:");
			print_message Verbose_medium   (ModelPrinter.string_of_pi0 model (Cartography.get_current_pi0()));
			
			let pi0 = ref (Cartography.get_current_pi0()) in
			
(* 			counter_worker_working#start;    *)
			while (!more_pi0 && not !limit_reached) do 			    
				
				print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] pi0:");
				print_message Verbose_medium   (ModelPrinter.string_of_pi0 model !pi0);
				
				(*send_update_request();*)
				pi0 := (Cartography.get_current_pi0());
				
				
				send_pi0_worker !pi0;
				
				print_message Verbose_medium (" send pi0 to master ");
				
				let receivedContinue = ref false in
				
				while (not !receivedContinue) do
			
					let check = receive_work () in
					match check with
					
					| TileUpdate tile -> 		print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] received Tile from Master.");
(* 									let b = Cartography.bc_process_im_result tile in *)
								print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] received Tile from Master.");
								
					| Subpart subpart ->	print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] received scaled subpart tag from Master.");
								Input.set_v0 subpart;
								Cartography.bc_initialize_subpart ();
					
					| Continue ->  		print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] received continue tag from Master.");
								receivedContinue := true;	
								print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] received Tile from Master.");
								
					| _ -> 			print_message Verbose_medium ("error!!! receive tile at worker side." ^ (string_of_int rank) ^ " ");
								raise (InternalError("error!!! receive tile at worker side."));
					
				done; (* end while *)
			
				(* Set the new pi0 *)
				Input.set_pi0 !pi0;
			
				(* Save verbose mode *)
				let global_verbose_mode = get_verbose_mode() in 

				
				(* Prevent the verbose messages (except in verbose mode total) *)
				if not (verbose_mode_greater Verbose_total) then
					set_verbose_mode Verbose_mute;
				
				counter_worker_IM#start;
				try(
				
				(* Compute IM *)
				(*counter_worker_IM#start;*)
				let im_result , _ = Reachability.inverse_method_gen model init_state in
(* 			    raise (InternalError("stop here")); *)

				(* Get the verbose mode back *)
				set_verbose_mode global_verbose_mode;
				
				(* Process result *)
				let added = Cartography.bc_process_im_result im_result in
				counter_worker_IM#stop;
				
				(*** NOTE for the collaborator version: keep it in memory 
					all_tiles := im_result :: !all_tiles;
				***)
				
				(*send result to master*)
				send_result im_result;

				(* Print some info *)
				print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "]  Constraint really added? " ^ (string_of_bool added) ^ "");
				compute_next_pi0_sequentially more_pi0 limit_reached first_point (Some im_result.tile_nature);
				)
				with KillIM ->(
				(*** TODO ***)
					print_message Verbose_medium "\n -------------Killed IM-----------------"; 
					compute_next_pi0_sequentially more_pi0 limit_reached first_point (None);
				);
				
				
			done;
			
		| Terminate -> 
				print_message Verbose_medium (" Terminate ");
				print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] I was just told to terminate work.");
				finished := true
			
		| _ -> 		print_message Verbose_medium ("error!!! not implemented.");
				raise (InternalError("not implemented."));
		
	done;



	(** THE END **)
	
	(* Stop global counter *)
	counter_worker_total#stop;

	(* Print some information *)
	let occupancy = (counter_worker_total#value -. counter_worker_waiting#value ) /. (counter_worker_total#value) *. 100. in
	print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] I'm done.");
	print_message Verbose_standard ("[Worker " ^ (string_of_int rank) ^ "] Number of unsuccessful points: " ^ (string_of_int (Cartography.get_nb_unsuccessful_points())) ^ "");
	print_message Verbose_standard ("[Worker " ^ (string_of_int rank) ^ "] Waiting time                 : " ^ (string_of_float (counter_worker_waiting#value)) ^ " s");
	print_message Verbose_standard ("[Worker " ^ (string_of_int rank) ^ "] Time spent on IM             : " ^ (string_of_float (counter_worker_IM#value)) ^ " s");
	print_message Verbose_standard ("[Worker " ^ (string_of_int rank) ^ "] Time to find next pi0        : " ^ (string_of_float (counter_worker_find_next_pi0#value)) ^ " s");
	print_message Verbose_standard ("[Worker " ^ (string_of_int rank) ^ "] Total time                   : " ^ (string_of_float (counter_worker_total#value)) ^ " s");
	print_message Verbose_standard ("[Worker " ^ (string_of_int rank) ^ "] Occupancy                    : " ^ (string_of_float occupancy) ^ " %");

;;



(****************************************************************)
(**        COLLABORATOR (static distribution)         *)
(****************************************************************)

(* Compute the subpart for collaborator # rank *)
(*** NOTE: for now, brute force method: compute ALL subpart, then select the subpart # rank ***)
let collaborator_compute_subpart rank =
	(* Get the v0 *)
	let v0 = Input.get_v0() in

	(*** WARNING: duplicate code ***)
	(******************Adjustable values********************)
	(* number of subpart want to initialize in the first time *)

	(*** WARNING: why "-1" ??? ***)
	
	let np = (DistributedUtilities.get_nb_nodes ()) -1 in
	(*******************************************************)
		
	(* Compute all subparts *)
	let subparts = intialize_Subparts v0 np in
	
	print_message Verbose_low ("[collaborator " ^ (string_of_int rank) ^ "] I computed " ^ (string_of_int (List.length subparts)) ^ " subpart" ^ (s_of_int (List.length subparts)) ^ ".");
	
	(* Select subpart # rank *)
	let subpart =
	try(
		List.nth subparts rank
	)with
	Failure f -> (
		raise (InternalError("Error when accessing element #" ^ (string_of_int rank) ^ " in the list of subparts."))
	)
	in
	(* Print some verbose information *)
	print_message Verbose_low ("[collaborator " ^ (string_of_int rank) ^ "] I picked up subpart #" ^ (string_of_int rank) ^ ".");
	(* Return subpart *)
	subpart


(* At the end, the coordinator (collaborator #0) receives all tiles, and handles the result *)
let collaborator_0_finalize () =
	print_message Verbose_standard ( "[Coordinator] I am collaborator " ^ (string_of_int masterrank) ^ " and I am now the coordinator.");
	
	(* Retrieve the input options *)
(* 	let options = Input.get_options () in *)

	let other_collaborators = get_nb_nodes () - 1 in
	let workers_done = ref 0 in
	

	print_message Verbose_low ("[Coordinator] Expecting to receive " ^ (string_of_int other_collaborators) ^ " results from my collaborators.");
	
	while !workers_done < other_collaborators do
		print_message Verbose_low ("[Coordinator] " ^ ( string_of_int ( other_collaborators - !workers_done )) ^ " collaborators left." );

		let pull_request = receive_pull_request () in
		begin
		match pull_request with
		| Tiles (source_rank, im_result_list) ->
			let nb_tiles = List.length im_result_list in
			print_message Verbose_standard( "\t[Coordinator] Received " ^ (string_of_int nb_tiles) ^ " tile" ^ (s_of_int nb_tiles) ^ " from worker " ^ (string_of_int source_rank ) ^ ".");
			(* Add all received tiles *)
			(*** TODO: measure processing time ***)
(* 							   	counter_master_processing#start; *)
			(*** TODO: count the number of useless / redundant tiles ***)
			List.iter (fun im_result -> 
				let _ = Cartography.bc_process_im_result im_result in
				print_message Verbose_medium( "\t[Coordinator] Tile processed.");
				()
			) im_result_list;
			(*** TODO: measure processing time ***)
(* 				   	counter_master_processing#stop; *)

		| _ -> raise (InternalError "[Coordinator] I only expect to receive a Tiles tag at this point.")
		end;
		(* Increment the number of workers that finished their job *)
		workers_done := !workers_done + 1;
		
	done;
		
	print_message Verbose_standard ("[Coordinator] All workers done" );

	(* Process the finalization *)
	print_message Verbose_standard ("[Coordinator] Finalizing the cartography..." );
	Cartography.bc_finalize ();
	
	(*** WARNING! this time remains 0....... (or is it normal???) ***)
	print_message Verbose_standard ("[Coordinator] Total waiting time     : " ^ (string_of_float (counter_master_waiting#value)) ^ " s");
	print_message Verbose_standard ("**************************************************");

	(* Generate the graphics *)
	Cartography.output_graphical_cartography (Some "_cart_patator");
	
	(* The end *)
	()


	
(* Send tiles to the coordinator *)
let collaborator_n_finalize all_tiles =
	(* Print some information *)
	let rank = get_rank() in
	print_message Verbose_low ("[collaborator " ^ (string_of_int rank) ^ "] About to send all my tiles to the coordinator...");

	(* Send all tiles to the coordinator #0 *)
	send_tiles all_tiles;
	
	(* Print some information *)
	print_message Verbose_low ("[collaborator " ^ (string_of_int rank) ^ "] Tiles sent! The end for me.");
	
	(* The end *)
	()


(** Implementation of coordinator (for static distribution);
	WARNING: only works with a number of nodes equal to a power of 2 *)
let collaborator () =
	let rank = get_rank() in
	let nb_nodes = get_nb_nodes() in

	(* First, the node # masterrank will issue a warning if the number of nodes is NOT a power of 2 *)
	if rank = masterrank && not (is_a_power_of_2 nb_nodes) then
		print_warning ("The number of nodes in the static distribution scheme must be a power of 2, but it is here equal to " ^ (string_of_int nb_nodes) ^ "; the behavior of " ^ Constants.program_name ^ " is NOT specified.");
	
	(* Get the model *)
	let model = Input.get_model() in

	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Backup the v0, so that the coordinator can put it back before finishing its work (and plotting the cartography) *)
	let original_v0 = Input.get_v0 () in
	
	(* Backup verbose mode *)
	let global_verbose_mode = get_verbose_mode() in

	(* Compute subpart for this collaborator only *)
	let subpart = collaborator_compute_subpart rank in

	(* Assign subpart *)
	Input.set_v0 subpart;
	
	print_message Verbose_high ("[collaborator " ^ (string_of_int rank) ^ "] Subpart assigned.");
	
	(* Compute the initial state *)
	let init_state = Reachability.get_initial_state_or_abort model in
	
	print_message Verbose_high ("[collaborator " ^ (string_of_int rank) ^ "] Initial state accessed.");
	
	(* Maintain list of tiles for the finalization (send them all to the coordinator *)
	let all_tiles = ref [] in
	
	(* Initialize the cartography *)
	Cartography.bc_initialize ();
	
	(*** BADPROG: all the following is VERY close to Cartography.cover_behavioral_cartography; this function cannot be reused though, because it maintains only the constraints, whereas we need here the full "im_result" structure, to send them to the coordinator at the end.
		A solution could be to modify Cartography.cover_behavioral_cartography so that it maintains a list of im_result, and then reuse it.
		Close to this problem is the replacement of the im_result structure with an object that would be easier to handle.
	***)
		
	print_message Verbose_high ("[collaborator " ^ (string_of_int rank) ^ "] BEFORE pi0 computed.");
	
	(* Compute the first point pi0 *)
	Cartography.compute_initial_pi0 ();
	
	print_message Verbose_high ("[collaborator " ^ (string_of_int rank) ^ "] Initial pi0 computed.");
	
	let more_pi0 = ref true in
	let time_limit_reached = ref false in
	
	let current_iteration = ref 1 in
	
	(* Start main loop *)
	while !more_pi0 && not !time_limit_reached do
		print_message Verbose_low ("[collaborator " ^ (string_of_int rank) ^ "] BC, iteration " ^ (string_of_int !current_iteration) ^ ".");

		let pi0 = Cartography.get_current_pi0 () in

		print_message Verbose_high ("[collaborator " ^ (string_of_int rank) ^ "] Setting pi0...");

		(* Set the new pi0 *)
		Input.set_pi0 (pi0);
		
		print_message Verbose_high ("[collaborator " ^ (string_of_int rank) ^ "] About to call IM...");

		(* Prevent the verbose messages (except in verbose medium, high or total) *)
		if not (verbose_mode_greater Verbose_medium) then
			set_verbose_mode Verbose_mute;
		
		(* Call the inverse method *)
		let im_result, _ = Reachability.inverse_method_gen model init_state in
		
		(* Get the verbose mode back *)
		set_verbose_mode global_verbose_mode;
		
		print_message Verbose_high ("[collaborator " ^ (string_of_int rank) ^ "] Processing result of IM...");

		(* Process the result by IM *)
		let _ = Cartography.bc_process_im_result im_result in ();
		
		(* Add the result to the local list of tiles *)
		(*** TODO: only add to list if bc_process_im_result returned true? ***)
		all_tiles := im_result :: !all_tiles;
		
		print_message Verbose_high ("[collaborator " ^ (string_of_int rank) ^ "] Computing next pi0...");

		(* Compute the next pi0 (note that current_pi0 is directly modified by the function!) and return flags for more pi0 and co *)
		let found_pi0 , _ = Cartography.find_next_pi0 (Some im_result.tile_nature) in
		
		(* Update the found pi0 flag *)
		more_pi0 := found_pi0;
		
		print_message Verbose_medium ("[collaborator " ^ (string_of_int rank) ^ "] Iteration " ^ (string_of_int !current_iteration) ^ " completed.");

		(* Go to next iteration *)
		current_iteration := !current_iteration + 1;

	done; (* while more pi0 *)

	(* Print info if premature termination *)
	if !time_limit_reached && !more_pi0 then (
		(*** WARNING : what about other limits?! (iterations, etc.?) ***)
		match options#time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then print_warning (
				"The time limit (" ^ (string_of_int limit) ^ " second" ^ (s_of_int limit) ^ ") has been reached. The behavioral cartography algorithm now stops, although the cartography has not been covered yet."
			);
	);
	(* End main loop *)
	
	(* Do not process the finalization (useless, and the coordinator will do it itself ) *)
(* 	Cartography.bc_finalize (); *)
	
	(* Finalization depending on MPI rank *)
	if get_rank () = masterrank
		then (
			(* Put back the original v0 (especially to a cartography with the good V0) *)
			Input.set_v0 original_v0;
			(* Finalize *)
			collaborator_0_finalize()
		) else (
			collaborator_n_finalize !all_tiles
		)
	
	(* The end *)
