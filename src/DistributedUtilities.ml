(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre, Camille Coti
 * 
 * Created:       2014/03/24
 * Last modified: 2015/04/02
 *
 ****************************************************************)


(**************************************************)
(* External modules *)
(**************************************************)
open Mpi


(**************************************************)
(* Internal modules *)
(**************************************************)
open AbstractModel
open Exceptions
open CamlUtilities
open ImitatorUtilities
open Reachability


(****************************************************************)
(** Public types *)
(****************************************************************)
type rank = int

(** Worker tags *)
type pull_request =
	| PullOnly of rank
	| PullAndResult of rank * Reachability.im_result
	| OutOfBound of rank
	(*Hoang Gia new tags*)
	| Tile of rank * Reachability.im_result  (*the same with PullAndResult*)
	| Pi0 of rank * AbstractModel.pi0 (*the same with work at Master tag*)
	| UpdateRequest of rank



(** Master tags *)
type work_assignment =
	| Work of AbstractModel.pi0
	| Stop
	(*Hoang Gia new tags*)
	| Subpart of HyperRectangle.hyper_rectangle
		(*** TODO: how can the worker receive a tile from the master ??? ***)
	| Tile of Reachability.im_result
	| Terminate
	| Continue


type pi0_list = (Automaton.variable_index * NumConst.t) list

(****************************************************************)
(** Private types *)
(****************************************************************)
(** Tags sent by slave *)
type mpi_slave_tag =
	| Slave_result_tag (*Tile tag or constraint K*)
	| Slave_work_tag (*Pull tag*)
	| Slave_outofbound_tag (* out of bounded workers exception *)
	(*Hoang Gia new tags*)
	| Slave_tile_tag
	| Slave_pi0_tag
	| Slave_updaterequest_tag

(** Tags sent by master *)
type mpi_master_tag =
	| Master_data_tag (*pi0*)
	| Master_finished_tag (*Stop tags*)
	(*Hoang Gia new tags*)
	| Master_tile_tag
	| Master_subpart_tag
	| Master_terminate_tag
	| Master_continue_tag

(*	let data_tag = 1        (* used when we are sending input data           *)
let finished_tag = 2    (* used to mean that work is done                *)
let result_tag = 3      (* used when we are sending computation results  *)
let work_tag = 4        (* used to ask for some work                     *)
let outofbound_tag = 5  (* used by slave if out of time or depth         *)*)





(****************************************************************)
(** Constants *)
(****************************************************************)
(* Who is the master? *)
let masterrank = 0 



(****************************************************************)
(** Serialization Functions *)
(****************************************************************)
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
let serialize_SEP_LIST_IMRESULT = ";;"


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


let serialize_pi0 (pi0:AbstractModel.pi0) =
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
(* Result of IM *)
(*------------------------------------------------------------*)

let serialize_tile_nature = function
	| Good -> "G"
	| Bad -> "B"
	| Unknown -> "U"


let unserialize_tile_nature = function
	| "G" -> Good
	| "B" -> Bad
	| "U" -> Unknown
	| other -> raise (InternalError ("Impossible match '" ^ other ^ "' in unserialize_tile_nature."))


let serialize_returned_constraint = function
	(* Constraint under convex form *)
	| Convex_constraint (p_linear_constraint , tile_nature) ->
		(* Serialize the constraints *)
		(LinearConstraint.serialize_linear_constraint p_linear_constraint)
		^ serialize_SEP_PAIR
		(* Serialize the tile nature *)
		^ (serialize_tile_nature tile_nature)
	
	(* Disjunction of constraints *)
	| Union_of_constraints (p_linear_constraint_list , tile_nature) ->
		(* Serialize the list of constraints *)
		String.concat serialize_SEP_LIST  (List.map LinearConstraint.serialize_linear_constraint p_linear_constraint_list)
		^ serialize_SEP_PAIR
		(* Serialize the tile nature *)
		^ (serialize_tile_nature tile_nature)

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
	let tile_nature = unserialize_tile_nature tile_nature_str in
	(* Return *)
	let result =
	match constraints with
		| [p_linear_constraint] -> Convex_constraint (p_linear_constraint , tile_nature)
		| _ -> Union_of_constraints (constraints , tile_nature)
		(*** WARNING: NNCConstraint case not implemented ! ***)
	in result 
	

let serialize_im_result im_result =
	(* Returned constraint *)
	(serialize_returned_constraint im_result.result)
	^
	serialize_SEP_STRUCT
	^
	(* Tile nature *)
	(serialize_tile_nature im_result.tile_nature)
	^
	serialize_SEP_STRUCT
	^
	(* Premature stop? *)
	(string_of_bool im_result.premature_stop)
	^
	serialize_SEP_STRUCT
	^
	(* Deterministic analysis? *)
	(string_of_bool im_result.deterministic)
	^
	serialize_SEP_STRUCT
	^
	(* Number of states *)
	(string_of_int im_result.nb_states)
	^
	serialize_SEP_STRUCT
	^
	(* Number of transitions *)
	(string_of_int im_result.nb_transitions)
	^
	serialize_SEP_STRUCT
	^
	(* Number of iterations *)
	(string_of_int im_result.nb_iterations)
	^
	serialize_SEP_STRUCT
	^
	(* Computation time *)
	(string_of_float im_result.total_time)


let unserialize_im_result im_result_string =
	print_message Verbose_medium ( "[Master] About to unserialize '" ^ im_result_string ^ "'");
	let returned_constraint_string , tile_nature_str , premature_stop_string ,  deterministic_string , nb_states_string , nb_transitions_string , nb_iterations_string , total_time_string =
	match split serialize_SEP_STRUCT im_result_string with
		| [returned_constraint_string ; tile_nature_str ; premature_stop_string ; deterministic_string ; nb_states_string ; nb_transitions_string ; nb_iterations_string ; total_time_string ]
			-> returned_constraint_string , tile_nature_str , premature_stop_string , deterministic_string ,  nb_states_string , nb_transitions_string , nb_iterations_string , total_time_string
		| _ -> raise (SerializationError ("Cannot unserialize im_result '" ^ im_result_string ^ "'."))
	in
	{
		result 				= unserialize_returned_constraint returned_constraint_string;
		tile_nature			= unserialize_tile_nature tile_nature_str;
		premature_stop		= bool_of_string premature_stop_string;
		deterministic		= bool_of_string deterministic_string;
		nb_states			= int_of_string nb_states_string;
		nb_transitions		= int_of_string nb_transitions_string;
		nb_iterations		= int_of_string nb_iterations_string;
		total_time			= float_of_string total_time_string;
	}
	

(** Serialize a list of im_result *)
(*** NOTE: function implemented but not used now ***)
let serialize_im_result_list im_result_list =
	String.concat serialize_SEP_LIST_IMRESULT  (List.map serialize_im_result im_result_list)


(** Convert a list of serialized im_result into a serialized list of im_result (ad-hoc function to save time in subparts handling) *)
let serialized_imresultlist_of_serializedimresult_list=
	String.concat serialize_SEP_LIST_IMRESULT


(** Unserialize a list of im_result *)
let unserialize_im_result_list im_result_list_string =
	(* Retrieve the list of im_result *)
	List.map unserialize_im_result (split serialize_SEP_LIST_IMRESULT im_result_list_string)


(*------------------------------------------------------------*)
(* Tests *)
(*------------------------------------------------------------*)

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
(*test_serialization();
abort_program();;*)
	

(****************************************************************)
(** MPI Functions *)
(****************************************************************)
(*** NOTE: le "ref 1" ne signifie rien du tout ***)
let weird_stuff() = ref 1



let int_of_slave_tag = function
	| Slave_result_tag -> 1
	| Slave_work_tag -> 2
	| Slave_outofbound_tag -> 3
	(*Hoang Gia new tags*)
	| Slave_tile_tag -> 4
	| Slave_pi0_tag -> 5
	| Slave_updaterequest_tag -> 6
	
let int_of_master_tag = function
	| Master_data_tag -> 17
	| Master_finished_tag -> 18
	(*Hoang Gia new tags*)
	| Master_tile_tag -> 19
	| Master_subpart_tag -> 20
	| Master_terminate_tag -> 21
	| Master_continue_tag -> 22
	

let slave_tag_of_int = function
	| 1 -> Slave_result_tag
	| 2 -> Slave_work_tag
	| 3 -> Slave_outofbound_tag
	(*Hoang Gia new tags*)
	| 4 -> Slave_tile_tag
	| 5 -> Slave_pi0_tag
	| 6 -> Slave_updaterequest_tag
	| other -> raise (InternalError ("Impossible match '" ^ (string_of_int other) ^ "' in slave_tag_of_int."))

let master_tag_of_int = function
	| 17 -> Master_data_tag 
	| 18 -> Master_finished_tag
	(*Hoang Gia new tags*)
	| 19 -> Master_tile_tag
	| 20 -> Master_subpart_tag
	| 21 -> Master_terminate_tag
	| 22 -> Master_continue_tag
	| other -> raise (InternalError ("Impossible match '" ^ (string_of_int other) ^ "' in master_tag_of_int."))


let get_nb_nodes () = Mpi.comm_size Mpi.comm_world
let get_rank () = Mpi.comm_rank Mpi.comm_world


(*
***NOTE : doesn't work with the constraints***
let serialize( data ) =
	(*** WARNING: mis "No_sharing" completement au pif ***)
	Marshal.to_string data [(*No_sharing; *)Closures]


let unserialize( str ) =
	Marshal.from_string str 0*)


let message_MAX_SIZE = 100

(* Sends a result (first the size then the constraint), by the slave *)
let send_result (*linear_constraint*)im_result =
	let rank = get_rank() in

	print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] Entering send_constraint");
	let mlc = (*LinearConstraint.serialize_linear_constraint linear_constraint *) serialize_im_result im_result in
	let res_size = String.length mlc in

	print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] Serialized constraint '" ^ mlc ^ "'");
	
	(* Send the result: 1st send my rank, then the data size, then the data *)
	print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] About to send the size (" ^ (string_of_int res_size) ^ ") of the constraint.");
	Mpi.send rank masterrank (int_of_slave_tag Slave_result_tag) Mpi.comm_world;
	Mpi.send res_size masterrank (int_of_slave_tag Slave_result_tag) Mpi.comm_world;
	Mpi.send mlc masterrank (int_of_slave_tag Slave_result_tag) Mpi.comm_world

(*	(*** HACK: cut the constraint to try to solve a strange bug with MPI ***)
	if res_size <= message_MAX_SIZE then(
		(* Normal situation *)
		print_message Verbose_high ("[Worker " ^ (string_of_int rank) ^ "] About to send a constraint.");
		Mpi.send mlc masterrank (int_of_slave_tag Slave_result_tag) Mpi.comm_world ;
		print_message Verbose_low ("[Worker " ^ (string_of_int rank) ^ "] Sent constraint '" ^ mlc ^ "'");
		()
	)else(
		(* Cutting situation *)
		print_message Verbose_low ("[Worker " ^ (string_of_int rank) ^ "] About to cut a constraint into smaller parts.");
		let remainder = res_size mod message_MAX_SIZE in
		let nb_parts = res_size / message_MAX_SIZE in
		print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] There will be " ^ (string_of_int nb_parts) ^ " parts and " ^ (if remainder = 0 then "no" else "a") ^ " remainder.");
		for i = 0 to nb_parts - 1 do
			(* Cut the string *)
			let substring = String.sub mlc (i * message_MAX_SIZE) message_MAX_SIZE in
			print_message Verbose_high ("[Worker " ^ (string_of_int rank) ^ "] About to send a piece of constraint.");
			Mpi.send substring masterrank (int_of_slave_tag Slave_result_tag) Mpi.comm_world ;
			print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] Sent piece of constraint #" ^ (string_of_int i) ^ " '" ^ substring ^ "'");
		done;
		
		(* Send the remainder if not null *)
		if remainder <> 0 then(
			(* Cut the string *)
			let substring = String.sub mlc (nb_parts * message_MAX_SIZE) remainder in
			print_message Verbose_high ("[Worker " ^ (string_of_int rank) ^ "] About to send the last piece of a constraint.");
			Mpi.send substring masterrank (int_of_slave_tag Slave_result_tag) Mpi.comm_world ;
			print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] Sent (last) piece of constraint #" ^ (string_of_int nb_parts) ^ " '" ^ substring ^ "'");
		);
 
		print_message Verbose_low ("[Worker " ^ (string_of_int rank) ^ "] Sent constraint '" ^ mlc ^ "'  in small pieces.");
		
 		()
	)
 *)
 
(*send tile = send result*)
let send_result_worker (*linear_constraint*)im_result =
	let rank = get_rank() in

	print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] Entering send_constraint");
	let mlc = (*LinearConstraint.serialize_linear_constraint linear_constraint *) serialize_im_result im_result in
	let res_size = String.length mlc in

	print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] Serialized constraint '" ^ mlc ^ "'");
	
	(* Send the result: 1st send my rank, then the data size, then the data *)
	print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] About to send the size (" ^ (string_of_int res_size) ^ ") of the constraint.");
	Mpi.send rank masterrank (int_of_slave_tag Slave_tile_tag) Mpi.comm_world;
	Mpi.send res_size masterrank (int_of_slave_tag Slave_tile_tag) Mpi.comm_world;
	Mpi.send mlc masterrank (int_of_slave_tag Slave_tile_tag) Mpi.comm_world
	
 
 (* Hoang Gia Sends a result (first the size then the constraint), by the master *)
let send_tile (*linear_constraint*)im_result slave_rank =
	(*let rank = rank() in*)

	(*print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] Entering send_constraint");*)
	let mlc = (*LinearConstraint.serialize_linear_constraint linear_constraint *) serialize_im_result im_result in
	let res_size = String.length mlc in

	(*print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] Serialized constraint '" ^ mlc ^ "'");*)
	
	(* Send the result: 1st send my rank, then the data size, then the data *)
	(*print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] About to send the size (" ^ (string_of_int res_size) ^ ") of the constraint.");*)
	(*Mpi.send rank slave_rank (int_of_master_tag Master_tile_tag) Mpi.comm_world;*)
	Mpi.send res_size slave_rank (int_of_master_tag Master_tile_tag) Mpi.comm_world;
	Mpi.send mlc slave_rank (int_of_master_tag Master_tile_tag) Mpi.comm_world
	

(* Sends a point (first the size then the point), by the master *)
let send_pi0 (pi0 : AbstractModel.pi0) slave_rank =
	let mpi0 = serialize_pi0 pi0 in
	let res_size = String.length mpi0 in
	
	(* Send the result: 1st send the data size, then the data *)
	Mpi.send res_size slave_rank (int_of_master_tag Master_data_tag) Mpi.comm_world;
	Mpi.send mpi0 slave_rank (int_of_master_tag Master_data_tag) Mpi.comm_world
(* 		Printf.printf "[%d] result %s sent" (rank()) result ;  *)
(* 	print_newline() *)


(* Sends a point (first the size then the point), by the slave *)
let send_pi0_worker pi0  =
	let rank = get_rank() in
	print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] Entering send_pi0");
	let mpi0 = serialize_pi0 pi0 in
	let res_size = String.length mpi0 in
	(* Send the result: 1st send the data size, then the data *)
	Mpi.send rank masterrank (int_of_slave_tag Slave_pi0_tag) Mpi.comm_world;
	Mpi.send res_size masterrank (int_of_slave_tag Slave_pi0_tag) Mpi.comm_world;
	Mpi.send mpi0 masterrank (int_of_slave_tag Slave_pi0_tag) Mpi.comm_world





let send_work_request () =
	Mpi.send (get_rank()) masterrank (int_of_slave_tag Slave_work_tag) Mpi.comm_world
	
let send_update_request () =
	Mpi.send (get_rank()) masterrank (int_of_slave_tag Slave_updaterequest_tag) Mpi.comm_world
	
	
(*Hoang Gia send subpart by the Master*)
let send_subpart (subpart : HyperRectangle.hyper_rectangle) slave_rank =
	let msubpart = serialize_hyper_rectangle subpart in
	let res_size = String.length msubpart in
	
	(* Send the subpart: 1st send the data size, then the data *)
	Mpi.send res_size slave_rank (int_of_master_tag Master_subpart_tag ) Mpi.comm_world;
	Mpi.send msubpart slave_rank (int_of_master_tag Master_subpart_tag) Mpi.comm_world
	

let receive_pull_request () =
  
  (* First receive the length of the data we are about to receive *)
  let (l, source_rank, tag) = 
    Mpi.receive_status Mpi.any_source Mpi.any_tag Mpi.comm_world
  in

  print_message Verbose_medium ("\t[Master] MPI status received from [Worker " ^ ( string_of_int source_rank) ^"]");
  print_message Verbose_medium ("\t[Master] Tag decoded from [Worker " ^ ( string_of_int source_rank) ^"] : " ^ ( string_of_int tag ) );

  let tag = slave_tag_of_int tag in  

  (* Is this a result or a simple pull ? *)
  match tag with
  | Slave_result_tag ->
     let s_rank = l in
     print_message Verbose_medium ("[Master] Received Slave_result_tag from " ^ ( string_of_int source_rank) );

     let len = Mpi.receive s_rank (int_of_slave_tag Slave_result_tag) Mpi.comm_world in

     print_message Verbose_medium ("[Master] Expecting a result of size " ^ ( string_of_int len) ^ " from [Worker " ^ (string_of_int s_rank) ^ "]" );

     (* receive the result itself *)
     let buff = String.create len in
     let res = ref buff in
     print_message Verbose_medium ("[Master] Buffer created with length " ^ (string_of_int len)^"");	
     res := Mpi.receive s_rank (int_of_slave_tag Slave_result_tag) Mpi.comm_world ;
     print_message Verbose_medium("[Master] received buffer " ^ !res ^ " of size " ^ ( string_of_int len) ^ " from [Worker "  ^ (string_of_int source_rank) ^ "]");	

			
     (* Get the constraint *)
     let im_result = unserialize_im_result !res in
     
     PullAndResult (s_rank , im_result)
		   
  (* Case error *)
  | Slave_outofbound_tag ->
     print_message Verbose_medium ("[Master] Received Slave_outofbound_tag");
     OutOfBound source_rank
		
  (* Case simple pull? *)
  | Slave_work_tag ->
     print_message Verbose_medium ("[Master] Received Slave_work_tag from [Worker " ^ ( string_of_int source_rank) ^ "] : " ^  ( string_of_int l ));
     PullOnly (* source_rank *) l
     
  | Slave_updaterequest_tag ->
     print_message Verbose_medium ("[Master] Received Slave_updaterequest_tag from [Worker " ^ ( string_of_int source_rank) ^ "] : " ^  ( string_of_int l ));
     UpdateRequest (* source_rank *) l
     
     
  (*Hoang Gia new tags*)  
  
  (* Tile tag  same with Slave_result_tag*)
  | Slave_tile_tag ->
     let s_rank = l in
     print_message Verbose_medium ("[Master] Received Slave_tile_tag from " ^ ( string_of_int source_rank) );
     let len = Mpi.receive s_rank (int_of_slave_tag Slave_tile_tag) Mpi.comm_world in
     print_message Verbose_medium ("[Master] Expecting a result of size " ^ ( string_of_int len) ^ " from [Worker " ^ (string_of_int s_rank) ^ "]" );
     (* receive the K itself *)
     let buff = String.create len in
     let res = ref buff in
     print_message Verbose_medium ("[Master] Buffer created with length " ^ (string_of_int len)^"");	
     res := Mpi.receive s_rank (int_of_slave_tag Slave_tile_tag) Mpi.comm_world ;
     print_message Verbose_medium("[Master] received buffer " ^ !res ^ " of size " ^ ( string_of_int len) ^ " from [Worker "  ^ (string_of_int source_rank) ^ "]");	
     (* Get the constraint *)
     let im_result = unserialize_im_result !res in
     Tile (s_rank , im_result)
  
  (* pi0 tags same as Master_data_tag*)
  | Slave_pi0_tag ->
    let s_rank = l in 
    print_message Verbose_medium ("[Master] Received Slave_pi0_tag from " ^ ( string_of_int source_rank) );
    print_message Verbose_medium ("[Master] !!!!!!!!!!!!!!!!!!!!!1 " );
    let len = Mpi.receive s_rank (int_of_slave_tag Slave_pi0_tag) Mpi.comm_world in
    print_message Verbose_medium ("[Master] !!!!!!!!!!!!!!!!!!!!!1 " );
    print_message Verbose_medium ("[Master] Expecting a result of size " ^ ( string_of_int len) ^ " from [Worker " ^ (string_of_int s_rank) ^ "]" );
     (* Receive the data itself *)
    let buff = String.create len in
    let res = ref buff in
    print_message Verbose_medium ("[Master] Buffer created with length " ^ (string_of_int len)^"");	
    res := Mpi.receive s_rank (int_of_slave_tag Slave_pi0_tag) Mpi.comm_world ;
    print_message Verbose_medium("[Master] received buffer " ^ !res ^ " of size " ^ ( string_of_int len) ^ " from [Worker "  ^ (string_of_int source_rank) ^ "]");	
    (* Get the constraint *)
    let pi0 = (unserialize_pi0 !res) in
    Pi0 (s_rank , pi0)
    
;;



let send_finished source_rank = 
  print_message Verbose_medium( "[Master] Sending STOP to [Worker " ^ (string_of_int source_rank ) ^"].");
  Mpi.send (weird_stuff()) source_rank (int_of_master_tag Master_finished_tag) Mpi.comm_world 
  
(*Hoang Gia send TERMINATE tag*)
let send_terminate source_rank = 
  print_message Verbose_medium( "[Master] Sending TERMINATE to [Worker " ^ (string_of_int source_rank ) ^"].");
  Mpi.send (weird_stuff()) source_rank (int_of_master_tag Master_terminate_tag) Mpi.comm_world 
 
(*Hoang Gia send Continue tag*)
let send_continue source_rank = 
  print_message Verbose_medium( "[Master] Sending CONTINUE to [Worker " ^ (string_of_int source_rank ) ^"].");
  Mpi.send (weird_stuff()) source_rank (int_of_master_tag Master_continue_tag) Mpi.comm_world 

let receive_work () =
	(* Get the model *)
(* 	let model = Input.get_model() in *)

	let ( w, _, tag ) =
	Mpi.receive_status masterrank Mpi.any_tag Mpi.comm_world in

	let tag = master_tag_of_int tag in

	match tag with
	| Master_data_tag -> 
		(* Receive the data itself *)
		let buff = String.create w in
		let work = ref buff in

		work := Mpi.receive masterrank (int_of_master_tag Master_data_tag) Mpi.comm_world;
		
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

	| Master_finished_tag -> Stop
	
	
	(*Hoang Gia new tags*)
	| Master_tile_tag -> 
		(* Receive the data itself *)
		let buff1 = String.create w in
		let work1 = ref buff1 in

		work1 := Mpi.receive masterrank (int_of_master_tag Master_tile_tag) Mpi.comm_world;
		
		print_message Verbose_high ("Received " ^ (string_of_int w) ^ " bytes of work '" ^ !work1 ^ "' with tag " ^ (string_of_int (int_of_master_tag Master_tile_tag)));
		
		(* Get the K *)
		let im_result = unserialize_im_result !work1 in
		Tile im_result
		
	| Master_subpart_tag -> 
	  	(* Receive the data itself *)
		let buff2 = String.create w in
		let work2 = ref buff2 in

		work2 := Mpi.receive masterrank (int_of_master_tag Master_subpart_tag) Mpi.comm_world;
		
		print_message Verbose_high ("Received " ^ (string_of_int w) ^ " bytes of work '" ^ !work2 ^ "' with tag " ^ (string_of_int (int_of_master_tag Master_subpart_tag)));
		
		(* Get the K *)
		let subpart = unserialize_hyper_rectangle !work2 in
		Subpart subpart

	| Master_terminate_tag -> Terminate
	
	| Master_continue_tag -> Continue