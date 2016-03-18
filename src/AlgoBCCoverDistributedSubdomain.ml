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
 * Last modified     : 2016/03/18
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
(* Internal exceptions *)
(************************************************************)
(************************************************************)


(************************************************************)
(************************************************************)
(* Class-independent functions *)
(************************************************************)
(************************************************************)


(************************************************************)
(* Generic functions *)
(************************************************************)

(*** NOTE: code originally from DistributedMasterSlaveSubdomains.ml, written by Hoang Gia Nguyen (2014) ***)
(*** TODO: rewrite better (including exceptions) and move to OCamlUtilities ***)
(*remove element at*)	
let rec remove_at (lst : 'a list) (n : int) : 'a list=
	match lst with
	| [] -> [];
	| h :: t -> if n = 0 then t else h :: remove_at t (n-1);;
	
	
(************************************************************)
(* Simple functions *)
(************************************************************)
	
(*** NOTE: code originally from DistributedMasterSlaveSubdomains.ml, written by Hoang Gia Nguyen (2014) ***)
(*get total pi0 inside subdomain put the subdomain and number of dimensions then get the number of the pi0s inside it*) 
let getTotalPi0 (subdomain : HyperRectangle.hyper_rectangle) (d : int) = 
	let temp = ref 1 in
	for i = 0 to d do
		temp := !temp*( (NumConst.to_int(subdomain#get_max i) - NumConst.to_int(subdomain#get_min i) ) +1);
	done ;
	!temp


(************************************************************)
(* Splitting functions *)
(************************************************************)

(*** NOTE: code originally from DistributedMasterSlaveSubdomains.ml, written by Hoang Gia Nguyen (2014) ***)
let split s dimension = 
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
	if (!max_d_l = 1) then raise (InternalError ("the length is minimum, could not split smaller "));
	print_message Verbose_medium ("\ndetected Max dimension length in this subdomain is : " ^ (string_of_int (!max_d_l)) ^ " unit at dimension " ^ (string_of_int (dimension))); 
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
	[s1;s2]
	

(*** NOTE: code originally from DistributedMasterSlaveSubdomains.ml, written by Hoang Gia Nguyen (2014) ***)
(* Slipt subdomain put HyperRectangle s return List [s1;s2] !! the number of pi0 in subdomain must be larger or equals 2*)
let sliptLongestDimensionSubdomain (s : HyperRectangle.hyper_rectangle) =
	let d = HyperRectangle.get_dimensions() -1 in
	(* Sliptting subdomain into 2 smaller subdomains*)
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
	
	let listSubdomain = split s !max_d in

	listSubdomain
	
	
	

(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class virtual algoBCCoverDistributedSubdomain =
	object (self)
	inherit AlgoBCCoverDistributed.algoBCCoverDistributed as super
	
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* Rank of the collaborator (shortcut) *)
	val collaborator_rank = DistributedUtilities.get_rank()
	
	(* Number of collaborators (including the coordinator) *)
	val nb_collaborators = DistributedUtilities.get_nb_nodes ()
	
	(* The current algorithm instance *)
	(*** NOTE: this initialiation is useless (and time consuming?), as a new instance will be overwritten when needed ***)
(* 	val mutable current_bc_algo_instance : AlgoBCCover.algoBCCover = new AlgoBCCover.algoBCCover *)
	
	

	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual algorithm_name : string

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
(* 		super#initialize_variables; *)
		
		(* The end *)
		()


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Initialize the subdomains *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** WARNING: this function was originaly called with -1 for the number of collaborators ***)
	method compute_initial_subdomains =
		(* Get the v0 *)
		let v0 = Input.get_v0() in
		
		(*** NOTE: code originally from DistributedMasterSlaveSubdomains.ml, written by Hoang Gia Nguyen (2014) ***)
		let subdomains = ref [v0] in
		for l = 0 to nb_collaborators - 1 do 
		begin
		
	(* Find the largest subdomain to split *)
		let max_pi0s = ref 0 in
		let subno = ref 0 in
		for i = 0 to (List.length !subdomains)-1 do
			let temp = getTotalPi0 (List.nth !subdomains i) (HyperRectangle.get_dimensions() -1) in
			if (!max_pi0s < temp) then
			begin
			max_pi0s := temp; 
			subno := i;
			end
		done;
		(*check if length every edge if equals to unit*)
		if (!max_pi0s != 1) then 
		begin
			self#print_algo_message_newline Verbose_medium ("Max pi0s in list is : " ^ (string_of_int !max_pi0s) ^ " in subdomain : " ^ (string_of_int !subno));
			(*get list split subdomains*)
			let newSubdomainList = sliptLongestDimensionSubdomain (List.nth !subdomains !subno) in (*!subno*)
			(*remove old subdomain*)
			subdomains := (remove_at !subdomains !subno);
			(*add new subdomains*)
			subdomains := !subdomains @ newSubdomainList;
			self#print_algo_message_newline Verbose_medium ("List length : " ^ (string_of_int (List.length !subdomains) ) );
		end
		else
		begin
			raise (InternalError ("Could not split smaller and the requried Suparts/Workers larger the points in v0! Please Check Again!"));
		end;
		end
		done;
		
		(* Return the subdomains *)
		!subdomains

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Generic algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual run : unit -> Result.imitator_result

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual compute_bc_result : Result.imitator_result


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
