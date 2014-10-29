(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre, Camille Coti, Hoang Gia Nguyen
 * 
 * Created:       2014/09/05
 * Last modified: 2014/10/24
 *
 ****************************************************************)

 
(****************************************************************)
(* External modules *)
(****************************************************************)
open Mpi


(****************************************************************)
(* Internal modules *)
(****************************************************************)
open Exceptions
open ImitatorUtilities
open Options
open Reachability
open DistributedUtilities

(*Exception*)
exception Ex of string;;


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
	(*print_message Debug_standard ("\nSplitting............! ");
	print_message Debug_standard ("\ns infomation: ");
	print_message Debug_standard ("Number dimensions is : " ^ (string_of_int d) );
	for j = 0 to d do
	print_message Debug_standard ("Dimension " ^(string_of_int j)^" : "^ " min = " ^ (string_of_int (NumConst.to_int(s#get_min j)))^";"^ " max = " ^ (string_of_int (NumConst.to_int(s#get_max j))));
	done;
	(*check pi0 in subpart*)
	let totalpi0 = getTotalPi0 s d in
	print_message Debug_standard ("Total pi0s in s is : " ^ (string_of_int totalpi0) );*)
	(**********************************end printing***************************************************)
	let max_d_l = ref 0 in
	(*count from zero so that add 1 unit*)
	max_d_l := ( (NumConst.to_int(s#get_max dimension)) - (NumConst.to_int(s#get_min dimension)) +1 );
	if (!max_d_l = 1) then raise (Ex ("the length is minimum, could not split smaller "));
	print_message Debug_medium ("\ndetected Max dimension length in this subpart is : " ^ (string_of_int (!max_d_l)) ^ " unit at dimension " ^ (string_of_int (dimension))); 
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
	(*print_message Debug_standard ("\ns1 infomation: ");
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
	print_message Debug_standard ("Total pi0s in s2 is : " ^ (string_of_int totalpi0s2) );*)
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
	    print_message Debug_medium ("\nMax pi0s in list is : " ^ (string_of_int !max_pi0s) ^ " in subpart : " ^ (string_of_int !subno));
	    (*get list split subparts*)
	    let newSubpartList = sliptLongestDimensionSubpart (at !subparts !subno) in (*!subno*)
	    (*remove old subpart*)
	    subparts := (remove_at !subparts !subno);
	    (*add new subparts*)
	    subparts := !subparts@newSubpartList;
	    print_message Debug_medium ("\nList length : " ^ (string_of_int (List.length !subparts) ) );
	   end
	   else
	   begin
	    raise (Ex ("Could not split smaller and The requried Suparts/Workers larger the points in v0! Please Check Again! :P "));
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
(*let done_points_in_subpart (s : HyperRectangle.hyper_rectangle) (arr : AbstractModel.pi0) =
	let sum = ref 0 in
	let tail = ref 1 in
	for i= (HyperRectangle.get_dimensions()-1) downto 0 do
	  for j=i-1 downto 0 do
	    tail := !tail*(NumConst.to_int(s#get_max j) - NumConst.to_int(s#get_min j) +1) ;
	  done;
	  sum := !sum + ( ((NumConst.to_int (arr#get_value(i))) - NumConst.to_int(s#get_min j)) * !tail );
	  tail := 1;
	done;
	!sum +1;;
	()*)
	
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
	print_message Debug_medium ("\n entering dynamic splitting process" );
	let pi0 = get_next_sequential_pi0_in_subpart pi0 s in 
	let notFound = ref true in
	let max_d_l = ref 0 in
	let j = ref (HyperRectangle.get_dimensions()-1) in
	let lst = ref [] in
	print_message Debug_medium ("\n bug!!!!!!!!!!0" );
	while( !notFound (*&& (!j != -1)*) ) do
	  begin	
	  print_message Debug_medium ("\n bug!!!!!!!!!!1" );
	   (* if(!j = -1) then  print_message Debug_medium ("\n all demensions of subpart could not split" ); raise (Ex (" there are only 1 pi0 left in subpart, could not split! "));*)
	    (*if current pi0 at max dimension j but the Min at demension j of subpart is lower, split all the done pi0 below j*)
	    (*update subpart*)
	    print_message Debug_medium ("\n bug!!!!!!!!!!2" );
	    if ( NumConst.to_int(s#get_min (!j)) < pi0.(!j) ) then begin s#set_min (!j) (NumConst.numconst_of_int (pi0.(!j))) end;
	    print_message Debug_medium ("\n bug!!!!!!!!!!3" );
	    max_d_l := ( (NumConst.to_int(s#get_max (!j)) - pi0.(!j) ) +1 ) ;
	    print_message Debug_medium ("\n bug!!!!!!!!!!4" );
	    (*split subpart if the remain distance from pi0 to max dimension at leat 2 points*)
	    if( !max_d_l > 1 ) then
	      begin
	      print_message Debug_medium ("\n bug!!!!!!!!!!5" );
		print_message Debug_medium ("\nBegin split at demension : " ^ (string_of_int (!j) ) );
		lst := split s !j;
		notFound := false ;
	      end;
	    j := (!j - 1) ;
	   (* if(!j = -1) then
	      begin
		print_message Debug_medium ("\n all demensions of subpart could not split" );
		raise (Ex (" all demensions of subpart could not split "));
	      end;*)
	    
	    end(*end while*)
	  done;
	  (*if(!j = -1) then
	    begin
	      
	      print_message Debug_medium ("\n all demensions of subpart could not split" );
	      raise (Ex (" all demensions of subpart could not split "));
	    end;*)
	    print_message Debug_medium ("\n splitting list : " ^ (string_of_int (List.length !lst) ) );
	!lst;;
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
(* Some functions to implement *)
(*------------------------------------------------------------*)

(*------------------------------------------------------------*)
(* The cartography algorithm implemented in the master *)
(*------------------------------------------------------------*)
(*Hoang Gia master implementation*)
let master () =
	
	 (*Retrieve the input options *)
	let options = Input.get_options () in
	
	let check_covered = ref false in
	
	(* Get the model *)
	let model = Input.get_model() in
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
	
	print_message Debug_medium ("[Master] Hello world!");
	
	(* Perform initialization *)
	counter_master_processing#start;
	Cartography.bc_initialize ();
	counter_master_processing#stop;
	
	(* List of subparts maintained by the master *)
	let subparts = ref [] in
	
	let more_subparts = ref true in
	let limit_reached = ref false in
	

	(*print_message Debug_standard ("[Master] Here!!!!!!!!!!");*)
	(* create worker2subpart(worker,supart) *)
	let worker2subpart = ref [] in
	(* current pi0 of workers*)
	let current_Pi0 = ref [] in
	(* stopSplitting flag *)
	(*let stopSplitting = ref false in*)
	(*waitting *)
	(*let waittingList = ref [] in*)
	
	
	(******************Adjustable values********************)
	(* number of subpart want to initialize in the first time *)
	let np = 9 in
	(*depend on size of model*)
	let dynamicSplittingMode = ref true in
	(*******************************************************)
	
	(*initialize list of subpart*)
	counter_master_split#start;
	subparts := intialize_Subparts v0 np;
	counter_master_split#stop;
	
	let tilebuffer = ref [] in
	
	let pi0buffer = ref [] in
	
	let subpartbuffer = ref [] in

	
	(*** THE ALGORITHM STARTS HERE ***)
	while (not !check_covered) do
	  begin
		
		(* Get the pull_request *)
		(*let source_rank, tile_nature_option, pi0 = (receive_pull_request_subpart()) in*)
		print_message Debug_medium ("[Master] heloooooo ");
		
		counter_master_waiting#start;
		let pull_request = receive_pull_request() in
		counter_master_waiting#stop;
		
		(*send_terminate source_rank;*)
		match pull_request with 
		(*Pull Tag*)
		| PullOnly source_rank -> (
				 print_message Debug_medium ("[Master] Received a pull request from worker " ^ (string_of_int source_rank) ^ "");
				 (* check to delete if the worker comeback *)
				 if(List.mem_assoc source_rank !worker2subpart) then
				  begin
				    worker2subpart := List.remove_assoc source_rank !worker2subpart;
				  end;
				  (*******************)
				  if (!subparts != [])  then 
				  begin
				    (*send new subpart *)
				    send_subpart (List.hd !subparts) source_rank;
				    print_message Debug_medium ("[Master] Sent Subpart to worker " ^ (string_of_int source_rank) ^ "");
				    (*add into worker2subpart*)
				    worker2subpart := !worker2subpart@[( source_rank, (List.hd !subparts) )];
				    subparts := List.tl !subparts;
				  end
				 (*have not any subpart in list -> splitting* , if could not split -> terminate*)
				 else
				  begin
					
					(*chossing subpart to split*)
					print_message Debug_standard ("[Master] Splitting ....... ");	
					let s = ref (new HyperRectangle.hyper_rectangle) in
					let w = ref 0 in
					let pi0 = ref (new PVal.pval) in
					let remain_points = ref 0 in
					let temp = ref 0 in
					
					let subpartfound = ref false in
					let i = ref 0 in
					while( (not !subpartfound) && (!i != (List.length !worker2subpart) ) ) do
					(*for i = 0 to (List.length !worker2subpart) -1 do*)
					  begin
					  print_message Debug_standard ("[Master]  bugggggggggg!!!!!!!!!!!!! " );
					    let s_temp = (second (List.nth !worker2subpart !i)) in
					    let w_temp = (first (List.nth !worker2subpart !i)) in
					    let pi0_temp = (List.assoc w_temp !pi0buffer) in
					    let max_size = get_points_in_subpart s_temp in
					    let done_points = done_points_in_subpart s_temp pi0_temp in
					    temp := max_size - done_points;
					    
					    print_message Debug_standard ("[Master] temp " ^ (string_of_int !temp) ^ "");
					    print_message Debug_standard ("[Master] max_size " ^ (string_of_int max_size) ^ "");
					    print_message Debug_standard ("[Master] done_points " ^ (string_of_int done_points) ^ "");
					    
					    
					    let found_pi0 = ref false in
					    Cartography.test_pi0_uncovered !pi0 found_pi0 ;
					    if ((*!temp > !remain_points*) (*&& !found_pi0*) !found_pi0 && (!temp > 1) && (max_size > done_points) && (pi0InV0 pi0_temp s_temp) )
					    then
					     begin
					     print_message Debug_standard ("[Master]  bugggggggggg!!!!!!!!!!!!!1111111111 " );
						subpartfound := true;
						remain_points := !temp;
						s := s_temp;
						w := w_temp;
						pi0 := pi0_temp;
					     end;
					     print_message Debug_medium ("[Master] finding ....... ");
					     i := !i + 1;
					  end
					done;
					
					
					
					(*splitting*)
					 (*let found_pi0 = ref false in
					    Cartography.test_pi0_uncovered !pi0 found_pi0 ;*)
					if(!remain_points > 1 (*&& !found_pi0*)) 
					  then
					    begin
					    print_message Debug_medium ("[Master] Splitting ....... ");
					    worker2subpart := (List.remove_assoc !w !worker2subpart);
					    
					    if(List.mem_assoc !w !subpartbuffer) then
					    begin
					      subpartbuffer := List.remove_assoc !w !subpartbuffer;
					    end;
					    
					    let pi0arr = pval2array !pi0 in

					    (*counter_master_split#start;*)
					    print_message Debug_standard (ModelPrinter.string_of_v0 model !s ^ "\n dfgfg");
					    print_message Debug_standard   (ModelPrinter.string_of_pi0 model (!pi0) ^"\n dfgfg");
					    
					    let newSubparts = dynamicSplitSubpart !s pi0arr in

					    (*counter_master_split#stop;*)
					    
					    (*send splitSubpart worker*)
					    send_subpart (at newSubparts 1) source_rank;
					    worker2subpart := !worker2subpart@[source_rank, (at newSubparts 1)];
					    print_message Debug_medium ("[Master] sent slipt subpart 2....... ");
					    
					    (*send back to this worker*)
					    print_message Debug_medium ("[Master] sent slipt subpart 1....... ");
					    (*sending back now store in buffer to send later*)
					    subpartbuffer := !subpartbuffer@[!w, (at newSubparts 0)];
					    worker2subpart := !worker2subpart@[!w, (at newSubparts 0)];
					    
					    (*worker2subpart := !worker2subpart@[source_rank, !s];*)
					    
					   
					    (*waittingList := remove_at_lst !waittingList 0;*)
					    print_message Debug_standard ("[Master] All workers done" );
					    (*send_terminate source_rank;*)

					    end
					  else
					    begin
					    send_terminate source_rank;
					   (* waittingList := !waittingList@[source_rank];*)
						 print_message Debug_standard ("[Master] ASFSDGGSD" );
					    end;
					    
					    
					   
					

				  end)
		(*Tile Tag*)
		| Tile (source_rank , tile) -> (
				   print_message Debug_medium ("[Master] Received a tile from worker " ^ (string_of_int source_rank) ^ "");
				   (*check duplicated tile*)
				   	counter_master_processing#start;
					let b = Cartography.bc_process_im_result tile in
				   	counter_master_processing#stop;

				   if(b) then
				    begin
				      (*receive tile then send to the other workers to update*)
				      for i = 0 to (List.length !worker2subpart)-1 do
					if ( (first (List.nth !worker2subpart i)) != source_rank ) then
					  begin
					   (* send_tile tile (first (List.nth !worker2subpart i));*)
					   tilebuffer := !tilebuffer@[(first (List.nth !worker2subpart i)), tile];
					  end;
				      done
				    end;)

		(*Pi0 Tag*)
		| Pi0 (source_rank , pi0) -> (
				print_message Debug_medium ("[Master] Received a pi0 from worker " ^ (string_of_int source_rank) ^ "");
				
				(*Update pi0s*)
				if(List.mem_assoc source_rank !pi0buffer) then
				 begin
				   pi0buffer := List.remove_assoc source_rank !pi0buffer;
				 end;
				pi0buffer := !pi0buffer@[( source_rank, pi0 )];  
				print_message Debug_standard ("[Master] pi0s list : " ^ (string_of_int (List.length !pi0buffer) ) ^ "");
				
				(*Send tiles for updating*)
				while(List.mem_assoc source_rank !tilebuffer) do
				  begin
				    send_tile (List.assoc source_rank !tilebuffer) source_rank;
				    tilebuffer := (List.remove_assoc source_rank !tilebuffer);
				    print_message Debug_standard ("[Master] send a tile to worker " ^ (string_of_int source_rank) ^ "");
				  end
				done;
				(*Send v0 for updating*)
				(*if(List.mem_assoc source_rank !worker2subpart) then
				begin
				  send_subpart (List.assoc source_rank !worker2subpart) source_rank;
				end;*)
				(*subpartbuffer*)
				if(List.mem_assoc source_rank !subpartbuffer) then
				begin
				  send_subpart (List.assoc source_rank !subpartbuffer) source_rank;
				  subpartbuffer := List.remove_assoc source_rank !subpartbuffer;
				 (* worker2subpart := List.remove_assoc source_rank !worker2subpart;*)
				end;
				
				send_continue source_rank)
				
				
		(*0ther cases*)
		| _ -> raise (InternalError("have not implemented."));
	  end;
	
	(**)
	if(!worker2subpart = []) 
	  then 
	  begin 
	    check_covered := true;
(*	    for i = 0 to (List.length !waittingList)-1 do
		send_terminate (List.nth !waittingList i);
	    done*)
	  end;
	
	(*stopSplitting := true;*)
	done;
	
	
	
	
	print_message Debug_standard ("[Master] All workers done" );

	(* Process the finalization *)
	counter_master_processing#start;
	Cartography.bc_finalize ();
	counter_master_processing#stop;
	
	(* Stop global counter *)
	counter_master_total#stop;

	(* Print some information *)
	let occupancy = (counter_master_total#value -. counter_master_waiting#value ) /. (counter_master_total#value) *. 100. in

	print_message Debug_standard ("[Master] Splitting time         : " ^ (string_of_float (counter_master_split#value)) ^ " s");
	print_message Debug_standard ("[Master] Processing time        : " ^ (string_of_float (counter_master_processing#value)) ^ " s");
	print_message Debug_standard ("[Master] Waiting time           : " ^ (string_of_float (counter_master_waiting#value)) ^ " s");
	print_message Debug_standard ("[Master] Occupancy              : " ^ (string_of_float occupancy) ^ " %");
	print_message Debug_standard ("**************************************************");
	
	(* Process the result and return *)
	let tiles = Cartography.bc_result () in
	(* Render zones in a graphical form *)
	if options#cart then (
		Graphics.cartography (Input.get_model()) (Input.get_v0()) tiles (options#files_prefix ^ "_cart_patator")
	) else (
		print_message Debug_high "Graphical cartography not asked: graph not generated.";
	);
	()


(****************************************************************)
(**        WORKER         *)
(****************************************************************)

(*------------------------------------------------------------*)
(* Generic function handling the next sequential point *)
(*------------------------------------------------------------*)
let compute_next_pi0_sequentially more_pi0 limit_reached first_point tile_nature_option =
	(* Start timer *)
	counter_worker_find_next_pi0#start ;
	
	(* Case first point *)
	if !first_point then(
		print_message Debug_low ("[Some worker] This is the first pi0.");
		Cartography.compute_initial_pi0 ();
		first_point := false;
		
	(* Other case *)
	)else(
		print_message Debug_low ("[Some worker] Computing next pi0 sequentially...");
		let found_pi0 , time_limit_reached = Cartography.find_next_pi0 tile_nature_option in
		(* Update the time limit *)
		limit_reached := time_limit_reached;
		(* Update the found pi0 flag *)
		more_pi0 := found_pi0;
	);
	
	(* Stop timer *)
	counter_worker_find_next_pi0#stop;
	(* The end *)
	()

let init_slave rank size =
	print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] I am worker " ^ (string_of_int rank) ^ "/" ^ (string_of_int (size-1)) ^ ".");
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
	
	let rank = Mpi.comm_rank Mpi.comm_world in
	let size = Mpi.comm_size Mpi.comm_world in
	init_slave rank size;
	let finished = ref false in
	(* In the meanwhile: compute the initial state *)
	let init_state = Reachability.get_initial_state_or_abort model in
	
	
	Cartography.bc_initialize ();


	while (not !finished) do
		send_work_request ();
		print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] sent pull request to the master.");
		counter_worker_waiting#start;

		let work = receive_work() in
		counter_worker_waiting#stop;
		match work with
		| Subpart subpart -> 
			print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] received subpart from Master.");
			
			
			(* To differentiate between initialization of pi0 / next_point *)
			let first_point = ref true in
			
			let more_pi0 = ref true in
			
			let limit_reached = ref false in
			
			(*initialize subpart*)
			Input.set_v0 subpart;
			
			(* Perform initialization *)
			Cartography.bc_initialize_subpart ();
			
			if debug_mode_greater Debug_medium then(
				print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] set v0:");
				print_message Debug_medium (ModelPrinter.string_of_v0 model subpart);
			);
			
			(*initial pi0*)
			Cartography.compute_initial_pi0();
			
			print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] Initial pi0:");
			print_message Debug_medium   (ModelPrinter.string_of_pi0 model (Cartography.get_current_pi0()));
			
			let pi0 = ref (Cartography.get_current_pi0()) in
			
(* 			counter_worker_working#start;    *)
 			while (!more_pi0 && not !limit_reached) do 			    
			    
			    print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] pi0:");
			    print_message Debug_medium   (ModelPrinter.string_of_pi0 model !pi0);
			    
			    (*send_update_request();*)
			    pi0 := (Cartography.get_current_pi0());
			    
			    
			    send_pi0_worker !pi0;
			    
			    print_message Debug_medium (" send_update_request to master ");
			    
			    let receivedContinue = ref false in
			     
			    while (not !receivedContinue) do
			  
			    let check = receive_work () in
			    match check with
				
			    | Tile tile -> 		print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] received Tile from Master.");
							let b = Cartography.bc_process_im_result tile in
							print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] received Tile from Master.");
							
			    | Subpart subpart ->	print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] received scaled subpart tag from Master.");
							if((done_points_in_subpart subpart !pi0) < 0)then
							begin
							  print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] bug!!!!!!!.");
							end
							else
							begin
							  Input.set_v0 subpart;
							  Cartography.bc_initialize_subpart ();
							end;
			    
			    | Continue ->  		print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] received continue tag from Master.");
							receivedContinue := true;	
							print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] received Tile from Master.");
							
			     | _ -> 			print_message Debug_medium ("error!!! receive tile at worker side." ^ (string_of_int rank) ^ " ");
							raise (InternalError("error!!! receive tile at worker side."));
			    
			    done;
			    
(* 				counter_worker_working#stop; *)
			    let found_pi0 = ref false in
			    Cartography.test_pi0_uncovered !pi0 found_pi0 ;
			    if(not !found_pi0) then
			    begin
				print_message Debug_medium ("[Workerq " ^ (string_of_int rank) ^ "] set v0:");
				print_message Debug_medium (ModelPrinter.string_of_v0 model (Input.get_v0()));
				print_message Debug_medium ("[Workerq " ^ (string_of_int rank) ^ "] Initial pi0:");
				print_message Debug_medium   (ModelPrinter.string_of_pi0 model (Cartography.get_current_pi0()));
				
			      compute_next_pi0_sequentially more_pi0 limit_reached first_point (None);
			       pi0 := (Cartography.get_current_pi0());
			    end;
							
			  
			    (* Set the new pi0 *)
			    Input.set_pi0 !pi0;
			
			    (* Save debug mode *)
			    let global_debug_mode = get_debug_mode() in 

			    
			    (* Prevent the debug messages (except in verbose modes high or total) *)
			    if not (debug_mode_greater Debug_total) 
			      then
				set_debug_mode Debug_nodebug;
			
			    (* Compute IM *)
			    counter_worker_IM#start;
			    let im_result , _ = Reachability.inverse_method_gen model init_state in
(* 			    raise (InternalError("stop here")); *)

			    (* Get the debug mode back *)
			    set_debug_mode global_debug_mode;
			    
			    (* Process result *)
			    let added = Cartography.bc_process_im_result im_result in
			    counter_worker_IM#stop;
			    
			   (* if(added) then
			    begin*)
			    (*send result to master*)
			      send_result_worker im_result;
			    (*end;*)
			    
			    (* Print some info *)
			    print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "]  Constraint really added? " ^ (string_of_bool added) ^ "");
			    

			    compute_next_pi0_sequentially more_pi0 limit_reached first_point (Some im_result.tile_nature);

			    (* Input.set_pi0 !pi0;*)
			     
			done;
		
(*		| Tile tile -> 	print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] I received subpart from Master.");
				let b = Cartography.bc_process_im_result tile in
				print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] received Tile from Master.");*)
			
		| Terminate -> 
				print_message Debug_medium (" Terminate ");
				print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] I was just told to terminate work.");
				finished := true
			
		| _ -> 		print_message Debug_medium ("have not implemented.");
				raise (InternalError("have not implemented."));
		
		
	done;
	
	(* Stop global counter *)
	counter_worker_total#stop;

	(* Print some information *)
	let occupancy = (counter_worker_total#value -. counter_worker_waiting#value ) /. (counter_worker_total#value) *. 100. in
	print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] I'm done.");
	print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] Number of unsuccessful points: " ^ (string_of_int (Cartography.get_nb_unsuccessful_points())) ^ "");
	print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] Waiting time                 : " ^ (string_of_float (counter_worker_waiting#value)) ^ " s");
	print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] Time spent on IM             : " ^ (string_of_float (counter_worker_IM#value)) ^ " s");
	print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] Time to find next pi0        : " ^ (string_of_float (counter_worker_find_next_pi0#value)) ^ " s");
	print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] Total time                   : " ^ (string_of_float (counter_worker_total#value)) ^ " s");
	print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] Occupancy                    : " ^ (string_of_float occupancy) ^ " %");

()
;;


(*------------------------------------------------------------*)
(* Tests *)
(*------------------------------------------------------------*)

(*implement the master*)
(*let test_gia () =

    

	print_message Debug_standard "--------------------Starting test !-------------------- \n"; 
	counter_master_waiting#start;
	(*************Sample Data v0************)

	HyperRectangle.set_dimensions 2;
	let v0 = new HyperRectangle.hyper_rectangle in 
	
(*	v0#set_min 0 (NumConst.numconst_of_int 0); 
	v0#set_max 0 (NumConst.numconst_of_int 3);
	v0#set_min 1 (NumConst.numconst_of_int 0);
	v0#set_max 1 (NumConst.numconst_of_int 3);
	v0#set_min 2 (NumConst.numconst_of_int 0);
	v0#set_max 2 (NumConst.numconst_of_int 3);*)
	
	v0#set_min 0 (NumConst.numconst_of_int 1); 
	v0#set_max 0 (NumConst.numconst_of_int 4);
	v0#set_min 1 (NumConst.numconst_of_int 1);
	v0#set_max 1 (NumConst.numconst_of_int 4);
	
	PVal.set_dimensions 2;
	let pi0 = new PVal.pval in
	pi0#set_value 0 (NumConst.numconst_of_int 2);
	pi0#set_value 1 (NumConst.numconst_of_int 2);
	
	
	(* List of subparts maintained by the master *)
(*	let subparts = ref [] in
	subparts := !subparts@[(v0)];

	print_message Debug_standard ("\nInitial list length : " ^ (string_of_int (List.length !subparts) ) );*)

	(*pi0*)
	(*let pi0 = [|0;2;2|] in*)
	(*let pi0 = [|4;4|] in*)
	
	
	
	(*let b = checkSplitCondition pi0 v0 in
	if(b) then  raise (Ex (" This Subpart do not satisfy condition! "));*)
	
	(*test split function*)
	(*split v0 2;*)
	
	(*test initialize subparts function*)
	(*subparts := (intialize_Subparts !subparts 500);*)
	
	(*test dynamicSplitSubpart*)
	(*subparts := dynamicSplitSubpart v0 pi0 ;*)
	
	(*check done points in subpart*)
	let done_points = done_points_in_subpart v0 pi0 in
	print_message Debug_standard ("\n done points : " ^ (string_of_int (done_points) ) );
	
	(*check points in subpart*)
	let points = get_points_in_subpart v0 in
	print_message Debug_standard ("\n points : " ^ (string_of_int (points) ) );
	
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
	print_message Debug_standard "\n --------------------End of test !--------------------"; 
	
()

;;
test_gia();
abort_program();*)
