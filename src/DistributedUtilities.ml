(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: All common functions needed for the interface with MPI
 * 
 * File contributors : Étienne André, Camille Coti
 * Created           : 2014/03/24
 * Last modified     : 2016/08/15
 *
 ************************************************************)
 

(************************************************************)
(* External modules *)
(************************************************************)
open Mpi


(************************************************************)
(* Internal modules *)
(************************************************************)
open AbstractModel
open Exceptions
open OCamlUtilities
open ImitatorUtilities
open Result


(************************************************************)
(** Public types *)
(************************************************************)
type rank = int

(** Tags sent by workers *)
type pull_request =
	| PullOnly of rank
	| Tile of rank * Result.abstract_im_result
	| OutOfBound of rank
	(* Subdomain tags *)
(* 	| Tiles of rank * (Result.abstract_im_result list) *)
(* 	| BC_result of rank * bc_result *)
	| Pi0 of rank * PVal.pval
	| UpdateRequest of rank


(** Tags sent by the master *)
type work_assignment =
	| Work of PVal.pval
	| Stop
	(* Subdomain tags *)
	| Subdomain of HyperRectangle.hyper_rectangle
	| TileUpdate of Result.abstract_im_result
	| Terminate
	| Continue


type pi0_list = (Automaton.variable_index * NumConst.t) list



(************************************************************)
(** Private types *)
(************************************************************)
(** Tags sent by slave *)
type mpi_slave_tag =
	| Slave_tile_tag (*Tile tag or constraint K*)
	| Slave_work_tag (*Pull tag*)
	| Slave_outofbound_tag (* out of bounded workers exception *)
	(* Subdomain tags *)
	| Slave_tiles_tag (** NEW TAG **)
	| Slave_bcresult_tag
	| Slave_pi0_tag
	| Slave_updaterequest_tag

(** Tags sent by master *)
type mpi_master_tag =
	| Master_data_tag (*pi0*)
	| Master_stop_tag (*Stop tags*)
	(* Subdomain tags *)
	| Master_tileupdate_tag
	| Master_subdomain_tag
	(*** NOTE: difference with Master_stop_tag ??? ***)
	| Master_terminate_tag
	| Master_continue_tag




(************************************************************)
(** Constants *)
(************************************************************)
(* Who is the master? (in a master-worker algorithm) *)
let master_rank = 0

(* Who is the coordinator? (in a collaborator-based algorithm) *)
let coordinator_rank = 0


(************************************************************)
(** Serialization Functions *)
(************************************************************)
(*------------------------------------------------------------*)
(* General *)
(*------------------------------------------------------------*)

let serialize_numconst = NumConst.string_of_numconst
let unserialize_numconst = NumConst.numconst_of_string

(* Reminder: list of separators in LinearConstraint:
	+ * a  
*)


(* Separator between the two elements of a pair *)
let serialize_SEP_PAIR = ","

(* Separator between the elements of a list *)
let serialize_SEP_LIST = ";"

(* Separator between the elements of a structure *)
let serialize_SEP_STRUCT = "|"

(* Separator between the elements of a list of im_results (need to be different from serialize_SEP_LIST because im_result contains itself some serialize_SEP_LIST *)
(*** WARNING: when using some symbols (e.g., "£" or two symboles like ";;") it does NOT work, and creates a list with alternating elements and empty string ***)
let serialize_SEP_LIST_IMRESULT = "#"

(* Separator between the elements of a structure containing itself structures *)
let serialize_SEP_SUPER_STRUCT = "@"


(*------------------------------------------------------------*)
(* Pi0 *)
(*------------------------------------------------------------*)

(*** WARNING / TODO : we should just send the VALUES, not the indexes !!! ***)

let serialize_pi0_pair (variable_index , value) =
	LinearConstraint.serialize_variable variable_index
	^
	serialize_SEP_PAIR
	^
	(serialize_numconst value)


let serialize_pi0 (pi0:PVal.pval) =
	let nb_parameters = PVal.get_dimensions () in
	(* Create an array *)
	let pi0_array = Array.create nb_parameters (0, NumConst.zero) in
	for parameter_index = 0 to nb_parameters - 1 do
		pi0_array.(parameter_index) <- parameter_index (*** WARNING: USELESS ***), pi0#get_value parameter_index;
	done;
	(* Convert to list *)
	let pi0_list = Array.to_list pi0_array in
	(* Convert all pairs to string *)
	let pi0_string_list = List.map serialize_pi0_pair pi0_list in
	(* Add separators *)
	String.concat serialize_SEP_LIST pi0_string_list

let unserialize_pi0_pair pi0_pair_string =
	match split serialize_SEP_PAIR pi0_pair_string with
	| [variable_string ; value_string ] ->
		LinearConstraint.unserialize_variable variable_string , unserialize_numconst value_string
	| _ -> raise (SerializationError ("Cannot unserialize pi0 value '" ^ pi0_pair_string ^ "': (variable_index, value) expected."))


let unserialize_pi0 pi0_string =
	(*** TODO: check correct number of values ! ***)
	(* Split into a list of pairs *)
	let pi0_pairs_string = split serialize_SEP_LIST pi0_string in
	(* Retrieve a list of (min, max) *)
	let pi0_list = List.map unserialize_pi0_pair pi0_pairs_string in
	(* Build the pi0 *)
	let pi0 = new PVal.pval in
	let parameter_index = ref 0 in
	List.iter (fun (_(* WARNING: USELESS *), value) ->
		pi0#set_value !parameter_index value;
		parameter_index := !parameter_index + 1;
	) pi0_list;
	(* Return *)
	pi0

	(*
	let pi0_pairs_string = split serialize_SEP_LIST pi0_string in
	List.map unserialize_pi0_pair pi0_pairs_string*)


(*------------------------------------------------------------*)
(* V0 *)
(*------------------------------------------------------------*)
let serialize_hyper_rectangle_pair (min, max) =
	(serialize_numconst min)
	^
	serialize_SEP_PAIR
	^
	(serialize_numconst max)

	
let serialize_hyper_rectangle hyper_rectangle =
	let nb_parameters = HyperRectangle.get_dimensions () in
	(* Create an array of pairs *)
	let hyper_rectangle_array = Array.create nb_parameters (NumConst.zero, NumConst.zero) in
	for parameter_index = 0 to nb_parameters - 1 do
		hyper_rectangle_array.(parameter_index) <- (hyper_rectangle#get_min parameter_index, hyper_rectangle#get_max parameter_index);
	done;
	(* Convert to list *)
	let hyper_rectangle_list = Array.to_list hyper_rectangle_array in
	(* Convert all pairs to string *)
	let hyper_rectangle_string_list = List.map serialize_hyper_rectangle_pair hyper_rectangle_list in
	(* Add separators *)
	String.concat serialize_SEP_LIST hyper_rectangle_string_list

let unserialize_hyper_rectangle_pair hyper_rectangle_pair_string =
	match split serialize_SEP_PAIR hyper_rectangle_pair_string with
	| [min_string ; max_string ] ->
		unserialize_numconst min_string , unserialize_numconst max_string
	| _ -> raise (SerializationError ("Cannot unserialize hyper_rectangle value '" ^ hyper_rectangle_pair_string ^ "': (min, max) expected."))


let unserialize_hyper_rectangle hyper_rectangle_string =
	(*** TODO: check correct number of values ! ***)
	(* Split into a list of pairs *)
	let hyper_rectangle_pairs_string = split serialize_SEP_LIST hyper_rectangle_string in
	(* Retrieve a list of (min, max) *)
	let hyper_rectangle_list = List.map unserialize_hyper_rectangle_pair hyper_rectangle_pairs_string in
	(* Build the hyper_rectangle *)
	let hyper_rectangle = new HyperRectangle.hyper_rectangle in
	let parameter_index = ref 0 in
	List.iter (fun (min, max) ->
		hyper_rectangle#set_min !parameter_index min;
		hyper_rectangle#set_max !parameter_index max;
		parameter_index := !parameter_index + 1;
	) hyper_rectangle_list;
	(* Return *)
	hyper_rectangle



(*------------------------------------------------------------*)
(* BFS result *)
(*------------------------------------------------------------*)

let serialize_statespace_nature = function
	| StateSpace.Good -> "G"
	| StateSpace.Bad -> "B"
	| StateSpace.Unknown -> "U"


let unserialize_statespace_nature = function
	| "G" -> StateSpace.Good
	| "B" -> StateSpace.Bad
	| "U" -> StateSpace.Unknown
	| other -> raise (InternalError ("Impossible match '" ^ other ^ "' in unserialize_statespace_nature."))

let serialize_bfs_algorithm_termination = function
	(* Fixpoint-like termination *)
	| Result.Regular_termination -> "R"
	(* Termination due to time limit reached *)
	| Result.Time_limit nb -> "T" ^ (string_of_int nb)
	(* Termination due to state space depth limit reached *)
	| Result.Depth_limit nb -> "D" ^ (string_of_int nb)
	(* Termination due to a number of explored states reached *)
	| Result.States_limit nb -> "S" ^ (string_of_int nb)

(*** TODO ***)
let unserialize_bfs_algorithm_termination = function
	(* Fixpoint-like termination *)
	| "R" -> Result.Regular_termination
	| other when other <> "" ->(
		(* Get first character *)
		let first_char = String.get other 0 in
		(* Get rest *)
		let rest = String.sub other 1 (String.length other - 1) in
		match first_char with
		| 'T' -> Result.Time_limit (int_of_string rest)
		| 'D' -> Result.Depth_limit (int_of_string rest)
		| 'S' -> Result.States_limit (int_of_string rest)
		| _ -> raise (InternalError ("Impossible match '" ^ other ^ "' in unserialize_bfs_algorithm_termination."))
	)
	| other -> raise (InternalError ("Impossible match '" ^ other ^ "' in unserialize_bfs_algorithm_termination."))
		


let serialize_constraint_soundness = function
	(* Constraint included in or equal to the real result *)
	| Result.Constraint_maybe_under -> "U"
	
	(* Exact result *)
	| Result.Constraint_exact -> "E"
	
	(* Constraint equal to or larger than the real result *)
	| Result.Constraint_maybe_over -> "O"
	
	(* Impossible to compare the constraint with the original result *)
	| Result.Constraint_maybe_invalid -> "I"
	
	| Result.Constraint_under_over -> raise (InternalError("BC is not suppose to handle under/over-approximations"))
	

let unserialize_constraint_soundness = function
	| "U" -> Result.Constraint_maybe_under
	| "E" -> Result.Constraint_exact
	| "O" -> Result.Constraint_maybe_over
	| "I" -> Result.Constraint_maybe_invalid
	| other -> raise (InternalError ("Impossible match '" ^ other ^ "' in unserialize_constraint_soundness."))


(*let serialize_returned_constraint = function
	(* Constraint under convex form *)
	| Convex_constraint (p_linear_constraint , tile_nature) ->
		(* Serialize the constraints *)
		(LinearConstraint.serialize_linear_constraint p_linear_constraint)
		^ serialize_SEP_PAIR
		(* Serialize the tile nature *)
		^ (serialize_statespace_nature tile_nature)
	
	(* Disjunction of constraints *)
	| Union_of_constraints (p_linear_constraint_list , tile_nature) ->
		(* Serialize the list of constraints *)
		String.concat serialize_SEP_LIST  (List.map LinearConstraint.serialize_linear_constraint p_linear_constraint_list)
		^ serialize_SEP_PAIR
		(* Serialize the tile nature *)
		^ (serialize_statespace_nature tile_nature)

	(* Non-necessarily convex constraint: set of constraints MINUS a set of negations of constraints *)
	| NNCConstraint _ -> raise (SerializationError ("Cannot serialize NNCConstraint yet."))



let unserialize_returned_constraint returned_constraint_string =
	(* Split between constraints and tile nature *)
	let constraints_str , tile_nature_str =
	match split serialize_SEP_PAIR returned_constraint_string with
	| [constraints_str ; tile_nature_str ] -> constraints_str , tile_nature_str
	| _ -> raise (SerializationError ("Cannot unserialize returned constraint '" ^ returned_constraint_string ^ "'."))
	in
	(* Retrieve the list of constraints *)
	let constraints = List.map LinearConstraint.unserialize_linear_constraint (split serialize_SEP_LIST constraints_str) in
	(* Unserialize tile nature *)
	let tile_nature = unserialize_statespace_nature tile_nature_str in
	(* Return *)
	let result =
	match constraints with
		| [p_linear_constraint] -> Convex_constraint (p_linear_constraint , tile_nature)
		| _ -> Union_of_constraints (constraints , tile_nature)
		(*** WARNING: NNCConstraint case not implemented ! ***)
	in result 
	*)

let serialize_abstract_state_space abstract_state_space =
	(* Number of states *)
	(string_of_int abstract_state_space.nb_states)
	^
	serialize_SEP_PAIR
	^
	(* Number of transitions *)
	(string_of_int abstract_state_space.nb_transitions)


let unserialize_abstract_state_space abstract_state_space_string =
	match split serialize_SEP_PAIR abstract_state_space_string with
	| [nb_states_str; nb_transitions_str] ->
		(* Abstract state space of IM for BC (to save memory) *)
		{
			nb_states			= int_of_string nb_states_str;
			nb_transitions		= int_of_string nb_transitions_str;
		}
	| _ -> raise (SerializationError ("Cannot unserialize abstract_state_space_string value '" ^ abstract_state_space_string ^ "'."))


let serialize_abstract_im_result abstract_im_result =
	(* Reference valuation *)
	(serialize_pi0 abstract_im_result.reference_val)
	^
	serialize_SEP_STRUCT
	^
	(* Convex constraint *)
	(LinearConstraint.serialize_p_convex_or_nonconvex_constraint abstract_im_result.result)
	^
	serialize_SEP_STRUCT
	^
	(* Abstracted version of the explored state space *)
	(serialize_abstract_state_space abstract_im_result.abstract_state_space)
	^
	serialize_SEP_STRUCT
	^
	(* Nature of the state space *)
	(serialize_statespace_nature abstract_im_result.statespace_nature)
	^
	serialize_SEP_STRUCT
	^
	(* Number of random selections of pi-incompatible inequalities performed *)
	(string_of_int abstract_im_result.nb_random_selections)
	^
	serialize_SEP_STRUCT
	^
	(* Total computation time of the algorithm *)
	(string_of_float abstract_im_result.computation_time)
	^
	serialize_SEP_STRUCT
	^
	(* Soundness of the result *)
	(serialize_constraint_soundness abstract_im_result.soundness)
	^
	serialize_SEP_STRUCT
	^
	(* Termination *)
	(serialize_bfs_algorithm_termination abstract_im_result.termination)




let unserialize_abstract_im_result abstract_im_result_string =

	print_message Verbose_high ( "[Master] About to unserialize '" ^ abstract_im_result_string ^ "'");
	let reference_val_str, result_str, abstract_state_space_str, statespace_nature_str, nb_random_selections_str , computation_time_str, soundness_str, termination_str =
	match split serialize_SEP_STRUCT abstract_im_result_string with
		| [reference_val_str; result_str; abstract_state_space_str; statespace_nature_str; nb_random_selections_str ; computation_time_str; soundness_str; termination_str ]
			-> reference_val_str, result_str, abstract_state_space_str, statespace_nature_str, nb_random_selections_str , computation_time_str, soundness_str, termination_str
		| _ -> raise (SerializationError ("Cannot unserialize im_result '" ^ abstract_im_result_string ^ "'."))
	in
	{
		reference_val 			= unserialize_pi0 reference_val_str;
		result 					= LinearConstraint.unserialize_p_convex_or_nonconvex_constraint result_str;
		abstract_state_space 	= unserialize_abstract_state_space abstract_state_space_str;
		statespace_nature		= unserialize_statespace_nature statespace_nature_str;
		nb_random_selections	= int_of_string nb_random_selections_str;
		computation_time		= float_of_string computation_time_str;
		soundness				= unserialize_constraint_soundness soundness_str;
		termination				= unserialize_bfs_algorithm_termination termination_str;
	}


(*------------------------------------------------------------*)
(* Cartography result *)
(*------------------------------------------------------------*)

(* Termination for cartography algorithms *)
let serialize_bc_termination = function
	(* Fixpoint-like termination *)
	| BC_Regular_termination -> "R"
	(* Termination due to a maximum number of tiles computed *)
	| BC_Tiles_limit -> "L"
	(* Termination due to time limit reached *)
	| BC_Time_limit -> "T"
	(* Termination due to several limits (only possible in distributed setting) *)
	| BC_Mixed_limit -> "M"


let unserialize_bc_termination  = function
	| "R" -> BC_Regular_termination
	| "L" -> BC_Tiles_limit
	| "T" -> BC_Time_limit
	| "M" -> BC_Mixed_limit
	| other -> raise (InternalError ("Impossible match '" ^ other ^ "' in unserialize_bc_termination."))

(* Termination for cartography algorithms *)
let serialize_bc_coverage = function
	(* Full coverage in all dimensions, including rational points *)
	| Coverage_full -> "F"
	(* At least all integers are covered, rationals perhaps not *)
	| Coverage_integer_complete -> "I"
	(* No indication of coverage *)
	| Coverage_unknown -> "U"

let unserialize_bc_coverage = function
	| "F" -> Coverage_full
	| "I" -> Coverage_integer_complete
	| "U" -> Coverage_unknown
	| other -> raise (InternalError ("Impossible match '" ^ other ^ "' in unserialize_bc_coverage."))

	
(** Serialize a list of abstract_im_result *)
let serialize_abstract_im_result_list abstract_im_result_list =
	String.concat serialize_SEP_LIST_IMRESULT (List.map serialize_abstract_im_result abstract_im_result_list)


(** Unserialize a list of im_result *)
let unserialize_abstract_im_result_list abstract_im_result_list_string =
	(* Retrieve the list of im_result *)
	let split_list = split serialize_SEP_LIST_IMRESULT abstract_im_result_list_string in
	
(*	(* DEBUG *)
	print_string "\n**********";
	print_string ("\n Splitting '" ^ im_result_list_string ^ "' using separator '" ^ serialize_SEP_LIST_IMRESULT ^ "'");
	let i = ref 1 in
	List.iter (fun l ->
		print_string ("\n" ^ (string_of_int !i) ^ " : "  ^ l);
		i := !i+1;
	) split_list;
	print_string "\n**********";*)
	
	List.map unserialize_abstract_im_result split_list




let serialize_bc_result bc_result =
	(* Number of points in V0 *)
	(serialize_numconst bc_result.size_v0)
	^
	serialize_SEP_SUPER_STRUCT
	^
	(* List of tiles *)
	(serialize_abstract_im_result_list bc_result.tiles)
	^
	serialize_SEP_SUPER_STRUCT
	^
	(* Total computation time of the algorithm *)
	(string_of_float bc_result.computation_time)
	^
	serialize_SEP_SUPER_STRUCT
	^
(*	(* Computation time to look for points *)
	(string_of_float bc_result.find_point_time)
	^
	serialize_SEP_SUPER_STRUCT
	^*)
	(* Number of points on which IM could not be called because already covered *)
	(string_of_int bc_result.nb_unsuccessful_points)
	^
	serialize_SEP_SUPER_STRUCT
	^
	(* Evaluation of the coverage of V0 by tiles computed by the cartography *)
	(serialize_bc_coverage bc_result.coverage)
	^
	serialize_SEP_SUPER_STRUCT
	^
	(* Termination *)
	(serialize_bc_termination bc_result.termination)


let unserialize_bc_result bc_result_string =
	print_message Verbose_high ("[Coordinator] About to unserialize '" ^ bc_result_string ^ "'");
	let size_v0_str, tiles_str, computation_time_str, (*find_point_time_str, *)nb_unsuccessful_points_str , coverage_str, termination_str =
	match split serialize_SEP_SUPER_STRUCT bc_result_string with
		| [size_v0_str; tiles_str; computation_time_str; (*find_point_time_str; *)nb_unsuccessful_points_str ; coverage_str; termination_str ]
			-> size_v0_str, tiles_str, computation_time_str, (*find_point_time_str, *)nb_unsuccessful_points_str , coverage_str, termination_str
		| _ -> raise (SerializationError ("Cannot unserialize im_result '" ^ bc_result_string ^ "'."))
	in
	{
		size_v0 				= NumConst.numconst_of_string size_v0_str;
		tiles 					= unserialize_abstract_im_result_list tiles_str;
		computation_time		= float_of_string computation_time_str;
(* 		find_point_time 		= float_of_string find_point_time_str; *)
		nb_unsuccessful_points	= int_of_string nb_unsuccessful_points_str;
		coverage				= unserialize_bc_coverage coverage_str;
		termination				= unserialize_bc_termination termination_str;
	}


(*------------------------------------------------------------*)
(* Old tests *)
(*------------------------------------------------------------*)

(*
;;

let test_split some_string sep =
	let split_list = split sep some_string in
	print_string "\n**********";
	print_string ("\n Splitting '" ^ some_string ^ "' using separator '" ^ sep ^ "'");
	let i = ref 1 in
	List.iter (fun l ->
		print_string ("\n" ^ (string_of_int !i) ^ " : "  ^ l);
		i := !i+1;
	) split_list;
	print_string "\n**********";
	()
in
test_split "sdffsf;dsfsfsdf" ";";
test_split "sdffsf;;dsfsfsdf" ";;";
test_split "sdffsf;dsfsfsdf;sdfgkjsdgkf" ";";
test_split "sdffsf;;dsfsfsdf;;zeurziur" ";;";

test_split "-1*0>-24a1*1g7a1*0g17,B|B|false|false|19|18|9|0.19293498993;-1*0>-17a1*0g8a1*0+1*1g24,B|B|false|false|13|12|9|0.145488977433" ";";

test_split "-1*0>-24a1*1g7a1*0g17,B|B|false|false|19|18|9|0.19293498993#-1*0>-17a1*0g8a1*0+1*1g24,B|B|false|false|13|12|9|0.145488977433" "#";

test_split "-1*0>-24a1*1g7a1*0g17,B|B|false|false|19|18|9|0.19293498993;;-1*0>-17a1*0g8a1*0+1*1g24,B|B|false|false|13|12|9|0.145488977433" ";;";

test_split "-1*0>-24a1*1g7a1*0g17,B|B|false|false|19|18|9|0.19293498993£-1*0>-17a1*0g8a1*0+1*1g24,B|B|false|false|13|12|9|0.145488977433" "£";


abort_program();;*)


(*------------------------------------------------------------*)
(* Tests *)
(*------------------------------------------------------------*)

(*
let debug_string_of_pi0 pi0 =
	let nb_parameters = PVal.get_dimensions () in
	(*** BADPROG ***)
	let my_string = ref "Pi0:" in
	for parameter_index = 0 to nb_parameters - 1 do
		my_string := !my_string ^ "\n"
			^ "p" ^ (string_of_int parameter_index)
			^ " => "
			^ (NumConst.string_of_numconst (pi0#get_value parameter_index))
		;
	done;
	(* Return *)
	!my_string

	
let debug_string_of_v0 v0 =
	let nb_parameters = HyperRectangle.get_dimensions () in
	(*** BADPROG ***)
	let my_string = ref "V0:" in
	for parameter_index = 0 to nb_parameters - 1 do
		my_string := !my_string ^ "\n"
			^ (NumConst.string_of_numconst (v0#get_min parameter_index))
			^ ", "
			^ (NumConst.string_of_numconst (v0#get_max parameter_index))
		;
	done;
	(* Return *)
	!my_string


let test_serialization () =
	let test_unserialize_variable variable_string = 
		try(
		let unserialized_variable = LinearConstraint.unserialize_variable variable_string in
		print_message Verbose_standard ("Unserializing " ^ variable_string ^ "...: " ^ (string_of_int unserialized_variable));
		) with
		SerializationError error -> print_error ("Serialization error: " ^ error)
	in
	test_unserialize_variable "0";
	test_unserialize_variable "1";
	test_unserialize_variable "26";
	test_unserialize_variable "184848448";
	test_unserialize_variable "";
	test_unserialize_variable "plouf";
	test_unserialize_variable "-2";
	test_unserialize_variable "3.2";
	test_unserialize_variable "3829t39";
	
(*	let mypi0 = [
		( 0 , NumConst.zero ) ;
		( 1 , NumConst.one ) ;
(* 		( 2 , NumConst.minus_one ) ; *)
		( 3 , NumConst.numconst_of_int 23) ;
(* 		( 4 , NumConst.numconst_of_int (-13)) ; *)
		( 5 , NumConst.numconst_of_frac 2 2011) ;
	] in*)
	let mypi0 = new PVal.pval in
	mypi0#set_value 0 NumConst.zero;
	mypi0#set_value 1 NumConst.one;
(* 	mypi0#set_value 2 NumConst.minus_one; *)
	mypi0#set_value 3 (NumConst.numconst_of_int 23);
(* 	mypi0#set_value 4 (NumConst.numconst_of_int (-13)); *)
	mypi0#set_value 5 (NumConst.numconst_of_frac 2 2011);

	print_message Verbose_standard "Here is my pi0";
	print_message Verbose_standard (debug_string_of_pi0 mypi0);
	
	print_message Verbose_standard "Now serializing it...";
	let pi0_serialized = serialize_pi0 mypi0 in
	print_message Verbose_standard "After serialization:";
	print_message Verbose_standard  pi0_serialized;
	
	print_message Verbose_standard "Now unserializing it...";
	let mypi0_back = unserialize_pi0 pi0_serialized in
	print_message Verbose_standard  (debug_string_of_pi0 mypi0_back);

	(*** BIG HACK because nb dimensions not set yet ***)
	let nb_parameters = 5 in
	HyperRectangle.set_dimensions nb_parameters;
	
	(* Create dummy v0 *)
	let v0 = new HyperRectangle.hyper_rectangle in
	
	(* Set dimensions *)
	for parameter_index = 0 to nb_parameters - 1 do
		(* Set to (p, 2*p + 1*)
		v0#set_min parameter_index (NumConst.numconst_of_int parameter_index);
		v0#set_max parameter_index (NumConst.numconst_of_int (2 * parameter_index + 1));
	done;
	
	print_message Verbose_standard "Here is my hyper rectangle";
	print_message Verbose_standard (debug_string_of_v0 v0);
	
	print_message Verbose_standard "Now serializing it...";
	let v0_serialized = serialize_hyper_rectangle v0 in
	print_message Verbose_standard "After serialization:";
	print_message Verbose_standard  v0_serialized;
	
	print_message Verbose_standard "Now unserializing it...";
	let myv0_back = unserialize_hyper_rectangle v0_serialized in
	print_message Verbose_standard  (debug_string_of_v0 myv0_back);
	()

;;
test_serialization();
abort_program();;*)
	

(************************************************************)
(** MPI Functions *)
(************************************************************)
(*** NOTE: le "ref 1" ne signifie rien du tout ***)
let weird_stuff() = ref 1



let int_of_slave_tag = function
	| Slave_tile_tag -> 1
	| Slave_work_tag -> 2
	| Slave_outofbound_tag -> 3
	(* Subdomain tags *)
	| Slave_tiles_tag -> 4
	| Slave_bcresult_tag -> 5
	| Slave_pi0_tag -> 6
	| Slave_updaterequest_tag -> 7
	(*** NOTE: unused match case (but safer!) ***)
(* 	| _ -> raise (InternalError ("Impossible match in int_of_slave_tag.")) *)


let int_of_master_tag = function
	| Master_data_tag -> 17
	| Master_stop_tag -> 18
	(*Hoang Gia new tags*)
	| Master_tileupdate_tag -> 19
	| Master_subdomain_tag -> 20
	| Master_terminate_tag -> 21
	| Master_continue_tag -> 22
	(*** NOTE: unused match case (but safer!) ***)
(* 	| _ -> raise (InternalError ("Impossible match in int_of_master_tag.")) *)
	

let worker_tag_of_int = function
	| 1 -> Slave_tile_tag
	| 2 -> Slave_work_tag
	| 3 -> Slave_outofbound_tag
	(* Subdomain tags *)
	| 4 -> Slave_tiles_tag
	| 5 -> Slave_bcresult_tag
	| 6 -> Slave_pi0_tag
	| 7 -> Slave_updaterequest_tag
	| other -> raise (InternalError ("Impossible match '" ^ (string_of_int other) ^ "' in worker_tag_of_int."))

let master_tag_of_int = function
	| 17 -> Master_data_tag 
	| 18 -> Master_stop_tag
	(*Hoang Gia new tags*)
	| 19 -> Master_tileupdate_tag
	| 20 -> Master_subdomain_tag
	| 21 -> Master_terminate_tag
	| 22 -> Master_continue_tag
	| other -> raise (InternalError ("Impossible match '" ^ (string_of_int other) ^ "' in master_tag_of_int."))




(************************************************************)
(** Public access functions *)
(************************************************************)

let get_nb_nodes () = Mpi.comm_size Mpi.comm_world
let get_rank () = Mpi.comm_rank Mpi.comm_world


(* Check if a node is the master (for master-worker scheme) *)
let is_master () =
	get_rank () = master_rank

(* Check if a node is the coordinator (for collaborator-based scheme) *)
let is_coordinator () =
	get_rank () = coordinator_rank



(*** TODO: separate Master and Worker functions ***)





(** Generic function to send something *)
let send_serialized_data recipient tag serialized_data =
	(* For information purpose *)
	let rank = get_rank() in

	print_message Verbose_high ("[Node " ^ (string_of_int rank) ^ "] Entering send_serialized_data");
	let data_size = String.length serialized_data in

	if verbose_mode_greater Verbose_high then(
		print_message Verbose_high ("[Node " ^ (string_of_int rank) ^ "] Serialized abstract_im_result '" ^ serialized_data ^ "'");
	);
	
	(* Send the result: 1st send the data size, then the data *)
	print_message Verbose_high ("[Node " ^ (string_of_int rank) ^ "] About to send the size (" ^ (string_of_int data_size) ^ ") of the data.");
	Mpi.send data_size recipient tag Mpi.comm_world;
	Mpi.send serialized_data recipient tag Mpi.comm_world


let send_abstract_im_result abstract_im_result =
	(* For information purpose *)
	let rank = get_rank() in

	let serialized_data = serialize_abstract_im_result abstract_im_result in
	
	print_message Verbose_high ("[Worker " ^ (string_of_int rank) ^ "] Serialized abstract_im_result '" ^ serialized_data ^ "'");
	
	(* Call generic function *)
	send_serialized_data master_rank (int_of_slave_tag Slave_tile_tag) serialized_data


(*let send_abstract_im_result_list abstract_im_result_list =
	(* For information purpose *)
	let rank = get_rank() in

	let serialized_data = serialize_abstract_im_result_list abstract_im_result_list in
	
	print_message Verbose_high ("[Worker " ^ (string_of_int rank) ^ "] Serialized abstract_im_result_list '" ^ serialized_data ^ "'");
	
	(* Call generic function *)
	send_serialized_data master_rank (int_of_slave_tag Slave_tiles_tag) serialized_data*)


let send_bc_result bc_result =
	(* For information purpose *)
	let rank = get_rank() in

	let serialized_data = serialize_bc_result bc_result in
	
	print_message Verbose_high ("[Worker " ^ (string_of_int rank) ^ "] Serialized serialize_bc_result '" ^ serialized_data ^ "'");
	
	(* Call generic function *)
	send_serialized_data master_rank (int_of_slave_tag Slave_bcresult_tag) serialized_data
	

(* Sends a point (first the size then the point), by the master *)
let send_pi0 (pi0 : PVal.pval) slave_rank =
	let serialized_data = serialize_pi0 pi0 in
	
	print_message Verbose_high ("[Master] Serialized pi0 '" ^ serialized_data ^ "'");
	
	(* Call generic function *)
	send_serialized_data slave_rank (int_of_master_tag Master_data_tag) serialized_data

(*	let mpi0 = serialize_pi0 pi0 in
	let res_size = String.length mpi0 in
	
	(* Send the result: 1st send the data size, then the data *)
	Mpi.send res_size slave_rank (int_of_master_tag Master_data_tag) Mpi.comm_world;
	Mpi.send mpi0 slave_rank (int_of_master_tag Master_data_tag) Mpi.comm_world*)



(*(* Sends a result (first the size then the constraint), by the slave *)
let send_abstract_im_result abstract_im_result =
	let rank = get_rank() in

	print_message Verbose_high ("[Worker " ^ (string_of_int rank) ^ "] Entering send_abstract_im_result");
	let mlc = serialize_abstract_im_result abstract_im_result in
	let res_size = String.length mlc in

	if verbose_mode_greater Verbose_high then(
		print_message Verbose_high ("[Worker " ^ (string_of_int rank) ^ "] Serialized abstract_im_result '" ^ mlc ^ "'");
	);
	
	(* Send the result: 1st send the data size, then the data *)
	print_message Verbose_high ("[Worker " ^ (string_of_int rank) ^ "] About to send the size (" ^ (string_of_int res_size) ^ ") of the abstract_im_result.");
	Mpi.send res_size master_rank (int_of_slave_tag Slave_tile_tag) Mpi.comm_world;
	Mpi.send mlc master_rank (int_of_slave_tag Slave_tile_tag) Mpi.comm_world*)

(* 
(** Sends a list of tiles from the worker to the master *)
let send_tiles im_result_list =
	let rank = get_rank() in

	print_message Verbose_high ("[Worker " ^ (string_of_int rank) ^ "] Entering send_tiles");
	let mlc = serialize_im_result_list im_result_list in
	let res_size = String.length mlc in

	if verbose_mode_greater Verbose_high then(
		print_message Verbose_high ("[Worker " ^ (string_of_int rank) ^ "] Serialized constraint list '" ^ mlc ^ "'");
	);
	
	(* Send the result: 1st send the data size, then the data *)
	print_message Verbose_high ("[Worker " ^ (string_of_int rank) ^ "] About to send the size (" ^ (string_of_int res_size) ^ ") of the constraint.");

	print_message Verbose_high ("[Worker " ^ (string_of_int rank) ^ "] Sending size");
	Mpi.send res_size master_rank (int_of_slave_tag Slave_tiles_tag) Mpi.comm_world;
	
	print_message Verbose_high ("[Worker " ^ (string_of_int rank) ^ "] Sending tiles");
	Mpi.send mlc master_rank (int_of_slave_tag Slave_tiles_tag) Mpi.comm_world*)

	

(** Master sends a tile update to a worker *)
let send_tileupdate abstract_im_result slave_rank =
	let serialized_data = serialize_abstract_im_result abstract_im_result in
	
	print_message Verbose_high ("[Master] Serialized abstract_im_result '" ^ serialized_data ^ "'");
	
	(* Call generic function *)
	send_serialized_data slave_rank (int_of_master_tag Master_tileupdate_tag) serialized_data


(* Function to send a point from a worker to the master *)
let send_point_to_master point =
	let serialized_data = serialize_pi0 point in
	
	print_message Verbose_high ("[Worker] Serialized pi0 '" ^ serialized_data ^ "'");
	
	(* Call generic function *)
	send_serialized_data master_rank (int_of_slave_tag Slave_pi0_tag) serialized_data
	

let send_work_request () =
	Mpi.send (get_rank()) master_rank (int_of_slave_tag Slave_work_tag) Mpi.comm_world
	
let send_update_request () =
	Mpi.send (get_rank()) master_rank (int_of_slave_tag Slave_updaterequest_tag) Mpi.comm_world
	
	
(*Hoang Gia send subdomain by the Master*)
let send_subdomain (subdomain : HyperRectangle.hyper_rectangle) slave_rank =
	let msubdomain = serialize_hyper_rectangle subdomain in
	let res_size = String.length msubdomain in
	
	(* Send the subdomain: 1st send the data size, then the data *)
	Mpi.send res_size slave_rank (int_of_master_tag Master_subdomain_tag ) Mpi.comm_world;
	Mpi.send msubdomain slave_rank (int_of_master_tag Master_subdomain_tag) Mpi.comm_world
	

(** Handle reception by the master *)
let receive_pull_request () =
  
  (* First receive the length of the data we are about to receive *)
  let (l, source_rank, tag) = 
    Mpi.receive_status Mpi.any_source Mpi.any_tag Mpi.comm_world
  in

  print_message Verbose_high ("\t[Master] MPI status received from [Worker " ^ ( string_of_int source_rank) ^"]");
  print_message Verbose_high ("\t[Master] Tag decoded from [Worker " ^ ( string_of_int source_rank) ^"] : " ^ ( string_of_int tag ) );

  let tag = worker_tag_of_int tag in  

  (* Is this a result or a simple pull ? *)
(*** TODO: factorize a bit ***)
  match tag with
  | Slave_tile_tag ->
     print_message Verbose_high ("[Master] Received Slave_tile_tag from " ^ ( string_of_int source_rank) );

     print_message Verbose_high ("[Master] Expecting a result of size " ^ ( string_of_int l) ^ " from [Worker " ^ (string_of_int source_rank) ^ "]" );

     (* receive the result itself *)
     let buff = String.create l in
     let res = ref buff in
     print_message Verbose_high ("[Master] Buffer created with length " ^ (string_of_int l)^"");	
     res := Mpi.receive source_rank (int_of_slave_tag Slave_tile_tag) Mpi.comm_world ;
     print_message Verbose_high("[Master] received buffer " ^ !res ^ " of size " ^ ( string_of_int l) ^ " from [Worker "  ^ (string_of_int source_rank) ^ "]");	
			
     (* Get the constraint *)
     let abstract_im_result = unserialize_abstract_im_result !res in
     
     Tile (source_rank , abstract_im_result)
		   
  | Slave_tiles_tag ->
		print_error "Tag 'Slave_tiles_tag' not implemented in receive_pull_request";
		raise (InternalError("Tag 'Slave_tiles_tag' not implemented in receive_pull_request"))
  (*
      print_message Verbose_high ("[Master] Received Slave_tiles_tag from " ^ ( string_of_int source_rank) );

     print_message Verbose_high ("[Master] Expecting a result of size " ^ ( string_of_int l) ^ " from [Worker " ^ (string_of_int source_rank) ^ "]" );

     (* receive the result itself *)
     let buff = String.create l in
     let res = ref buff in
     print_message Verbose_high ("[Master] Buffer created with length " ^ (string_of_int l)^"");	
     res := Mpi.receive source_rank (int_of_slave_tag Slave_tiles_tag) Mpi.comm_world ;
     print_message Verbose_high("[Master] received buffer " ^ !res ^ " of size " ^ ( string_of_int l) ^ " from [Worker "  ^ (string_of_int source_rank) ^ "]");	

			
     (* Get the constraint *)
     let im_result_list = unserialize_im_result_list !res in
     
     Tiles (source_rank , im_result_list)*)
		   
  (* Case error *)
  | Slave_outofbound_tag ->
     print_message Verbose_high ("[Master] Received Slave_outofbound_tag");
     OutOfBound source_rank
		
  (* Case simple pull? *)
  | Slave_work_tag ->
     print_message Verbose_high ("[Master] Received Slave_work_tag from [Worker " ^ ( string_of_int source_rank) ^ "] : " ^  ( string_of_int l ));
     PullOnly (* source_rank *) l
     
  | Slave_updaterequest_tag ->
     print_message Verbose_high ("[Master] Received Slave_updaterequest_tag from [Worker " ^ ( string_of_int source_rank) ^ "] : " ^  ( string_of_int l ));
     UpdateRequest (* source_rank *) l
     
     
  (*Hoang Gia new tags*)  
  
  (* pi0 tags same as Master_data_tag*)
  | Slave_pi0_tag ->
    print_message Verbose_high ("[Master] Received Slave_pi0_tag from " ^ ( string_of_int source_rank) );
    print_message Verbose_high ("[Master] Expecting a result of size " ^ ( string_of_int l) ^ " from [Worker " ^ (string_of_int source_rank) ^ "]" );
     (* Receive the data itself *)
    let buff = String.create l in
    let res = ref buff in
    print_message Verbose_high ("[Master] Buffer created with length " ^ (string_of_int l)^"");	
    res := Mpi.receive source_rank (int_of_slave_tag Slave_pi0_tag) Mpi.comm_world ;
    print_message Verbose_high("[Master] received buffer " ^ !res ^ " of size " ^ ( string_of_int l) ^ " from [Worker "  ^ (string_of_int source_rank) ^ "]");	
    (* Get the constraint *)
    let pi0 = (unserialize_pi0 !res) in
    Pi0 (source_rank , pi0)

	| Slave_bcresult_tag ->
		raise (InternalError("Cannot receive a Slave_bcresult_tag at that point"))
;;



let send_stop source_rank = 
  print_message Verbose_high( "[Master] Sending STOP to [Worker " ^ (string_of_int source_rank ) ^"].");
  Mpi.send (weird_stuff()) source_rank (int_of_master_tag Master_stop_tag) Mpi.comm_world 
  
(*Hoang Gia send TERMINATE tag*)
let send_terminate source_rank = 
  print_message Verbose_high( "[Master] Sending TERMINATE to [Worker " ^ (string_of_int source_rank ) ^"].");
  Mpi.send (weird_stuff()) source_rank (int_of_master_tag Master_terminate_tag) Mpi.comm_world 
 
(*Hoang Gia send Continue tag*)
let send_continue source_rank = 
  print_message Verbose_high( "[Master] Sending CONTINUE to [Worker " ^ (string_of_int source_rank ) ^"].");
  Mpi.send (weird_stuff()) source_rank (int_of_master_tag Master_continue_tag) Mpi.comm_world 


let receive_work () =
	(* Get the model *)
(* 	let model = Input.get_model() in *)

	let ( w, _, tag ) =
	Mpi.receive_status master_rank Mpi.any_tag Mpi.comm_world in

	let tag = master_tag_of_int tag in

	match tag with
	| Master_data_tag -> 
		(* Receive the data itself *)
		let buff = String.create w in
		let work = ref buff in

		work := Mpi.receive master_rank (int_of_master_tag Master_data_tag) Mpi.comm_world;
		
		print_message Verbose_high ("Received " ^ (string_of_int w) ^ " bytes of work '" ^ !work ^ "' with tag " ^ (string_of_int (int_of_master_tag Master_data_tag)));
		
		(* Get the pi0 *)
		let pi0 = unserialize_pi0 !work in
(*		(*** HACK ***)
		(* Convert back to an array *)
		let array_pi0 = Array.make model.nb_parameters NumConst.zero in
		List.iter (fun (variable_index, variable_value) ->
			array_pi0.(variable_index) <- variable_value;
		) pi0;
		(* Convert the pi0 to functional representation *)
		let pi0_fun = fun parameter -> array_pi0.(parameter) in*)
		Work (*pi0_fun*)pi0

	| Master_stop_tag -> Stop
	
	
	(*Hoang Gia new tags*)
	| Master_tileupdate_tag -> 
		(* Receive the data itself *)
		let buff1 = String.create w in
		let work1 = ref buff1 in

		work1 := Mpi.receive master_rank (int_of_master_tag Master_tileupdate_tag) Mpi.comm_world;
		
		print_message Verbose_high ("Received " ^ (string_of_int w) ^ " bytes of work '" ^ !work1 ^ "' with tag " ^ (string_of_int (int_of_master_tag Master_tileupdate_tag)));
		
		(* Get the result *)
		let abstract_im_result = unserialize_abstract_im_result !work1 in
		TileUpdate abstract_im_result
		
	| Master_subdomain_tag -> 
	  	(* Receive the data itself *)
		let buff2 = String.create w in
		let work2 = ref buff2 in

		work2 := Mpi.receive master_rank (int_of_master_tag Master_subdomain_tag) Mpi.comm_world;
		
		print_message Verbose_high ("Received " ^ (string_of_int w) ^ " bytes of work '" ^ !work2 ^ "' with tag " ^ (string_of_int (int_of_master_tag Master_subdomain_tag)));
		
		(* Get the K *)
		let subdomain = unserialize_hyper_rectangle !work2 in
		Subdomain subdomain

	| Master_terminate_tag -> Terminate
	
	| Master_continue_tag -> Continue


(* Function used for collaborator - coordinator static distribution scheme *)
let receive_bcresult () =
	(* First receive the length of the data we are about to receive *)
	let (l, source_rank, tag) = 
		Mpi.receive_status Mpi.any_source Mpi.any_tag Mpi.comm_world
	in

	print_message Verbose_high ("[Coordinator] MPI status received from Worker " ^ ( string_of_int source_rank) ^"");
	print_message Verbose_high ("[Coordinator] Tag decoded from Worker " ^ ( string_of_int source_rank) ^" : " ^ ( string_of_int tag ) );

	let tag = worker_tag_of_int tag in  

	(*** TODO: factorize a bit ***)
	match tag with
	| Slave_bcresult_tag ->
		print_message Verbose_high ("[Coordinator] Received Slave_bcresult_tag from " ^ ( string_of_int source_rank) );

		print_message Verbose_high ("[Coordinator] Expecting a result of size " ^ ( string_of_int l) ^ " from [Worker " ^ (string_of_int source_rank) ^ "]" );

		(* receive the result itself *)
		let buff = String.create l in
		let res = ref buff in
		print_message Verbose_high ("[Coordinator] Buffer created with length " ^ (string_of_int l)^"");	
		res := Mpi.receive source_rank (int_of_slave_tag Slave_bcresult_tag) Mpi.comm_world ;
		print_message Verbose_high("[Coordinator] received buffer " ^ !res ^ " of size " ^ ( string_of_int l) ^ " from Worker "  ^ (string_of_int source_rank) ^ "");
				
		(* Get the bc_result *)
		let bc_result = unserialize_bc_result !res in
		
		(* Return rank and result *)
		source_rank , bc_result
	
	| _ -> raise (InternalError("Unexpected tag received in receive_bcresult"))