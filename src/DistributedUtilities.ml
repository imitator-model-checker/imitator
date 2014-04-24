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
 * Last modified: 2014/04/23
 *
 ****************************************************************)

open Global
open Mpi
(* open Marshal *)
open AbstractModel
open Reachability


(****************************************************************)
(** Public types *)
(****************************************************************)
type rank = int

type pull_request =
	| PullOnly of rank
	| PullAndResult of rank * Reachability.im_result
	| OutOfBound of rank

type work_assignment =
	| Work of AbstractModel.pi0
	| Stop


type pi0_list = (Automaton.variable_index * NumConst.t) list

(****************************************************************)
(** Private types *)
(****************************************************************)
(** Tags sent by slave *)
type mpi_slave_tag =
	| Slave_result_tag
	| Slave_work_tag
	| Slave_outofbound_tag

(** Tags sent by master *)
type mpi_master_tag =
	| Master_data_tag
	| Master_finished_tag

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
(* Pi0 *)
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

let serialize_pi0_pair (variable_index , value) =
	LinearConstraint.serialize_variable variable_index
	^
	serialize_SEP_PAIR
	^
	(serialize_numconst value)


let serialize_pi0 pi0 =
	(* Convert all pairs to string *)
	let pi0_string_list = List.map serialize_pi0_pair pi0 in
	(* Add separators *)
	String.concat serialize_SEP_LIST pi0_string_list

let unserialize_pi0_pair pi0_pair_string =
	match split serialize_SEP_PAIR pi0_pair_string with
	| [variable_string ; value_string ] ->
		LinearConstraint.unserialize_variable variable_string , unserialize_numconst value_string
	| _ -> raise (SerializationError ("Cannot unserialize pi0 value '" ^ pi0_pair_string ^ "': (variable_index, value) expected."))


let unserialize_pi0 pi0_string =
	let pi0_pairs_string = split serialize_SEP_LIST pi0_string in
	List.map unserialize_pi0_pair pi0_pairs_string


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
	print_message Debug_medium ( "[Master] About to unserialize '" ^ im_result_string ^ "'");
	let returned_constraint_string , tile_nature_str , deterministic_string , nb_states_string , nb_transitions_string , nb_iterations_string , total_time_string =
	match split serialize_SEP_STRUCT im_result_string with
		| [returned_constraint_string ; tile_nature_str ; deterministic_string ; nb_states_string ; nb_transitions_string ; nb_iterations_string ; total_time_string ]
			-> returned_constraint_string , tile_nature_str , deterministic_string ,  nb_states_string , nb_transitions_string , nb_iterations_string , total_time_string
		| _ -> raise (SerializationError ("Cannot unserialize im_result '" ^ im_result_string ^ "'."))
	in
	{
	result 				= unserialize_returned_constraint returned_constraint_string;
	tile_nature			= unserialize_tile_nature tile_nature_str;
	deterministic		= bool_of_string deterministic_string;
	nb_states			= int_of_string nb_states_string;
	nb_transitions		= int_of_string nb_transitions_string;
	nb_iterations		= int_of_string nb_iterations_string;
	total_time			= float_of_string total_time_string;
	}
	
	

(*------------------------------------------------------------*)
(* Tests *)
(*------------------------------------------------------------*)

let debug_string_of_pi0 pi0 =
	"Pi0:"
	^ (String.concat "\n, "
		(List.map (fun (variable_index , value) ->
			"p" ^(string_of_int variable_index)
			^ " => "
			^ (NumConst.string_of_numconst value)
		) pi0 
		)
	)
	
let test_serialization () =
	let test_unserialize_variable variable_string = 
		try(
		let unserialized_variable = LinearConstraint.unserialize_variable variable_string in
		print_message Debug_standard ("Unserializing " ^ variable_string ^ "...: " ^ (string_of_int unserialized_variable));
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
	
	let mypi0 = [
		( 0 , NumConst.zero ) ;
		( 1 , NumConst.one ) ;
(* 		( 2 , NumConst.minus_one ) ; *)
		( 3 , NumConst.numconst_of_int 23) ;
(* 		( 4 , NumConst.numconst_of_int (-13)) ; *)
		( 5 , NumConst.numconst_of_frac 2 2011) ;
	] in
	print_message Debug_standard "Here is my pi0";
	print_message Debug_standard (debug_string_of_pi0 mypi0);
	
	print_message Debug_standard "Now serializing it...";
	let pi0_serialized = serialize_pi0 mypi0 in
	print_message Debug_standard "After serialization:";
	print_message Debug_standard  pi0_serialized;
	
	print_message Debug_standard "Now unserializing it...";
	let mypi0_back = unserialize_pi0 pi0_serialized in
	print_message Debug_standard  (debug_string_of_pi0 mypi0_back);
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

let int_of_master_tag = function
	| Master_data_tag -> 17
	| Master_finished_tag -> 18

let slave_tag_of_int = function
	| 1 -> Slave_result_tag
	| 2 -> Slave_work_tag
	| 3 -> Slave_outofbound_tag
	| other -> raise (InternalError ("Impossible match '" ^ (string_of_int other) ^ "' in slave_tag_of_int."))

let master_tag_of_int = function
	| 17 -> Master_data_tag 
	| 18 -> Master_finished_tag
	| other -> raise (InternalError ("Impossible match '" ^ (string_of_int other) ^ "' in master_tag_of_int."))


let size () = Mpi.comm_size Mpi.comm_world
let rank () = Mpi.comm_rank Mpi.comm_world


(*
***NOTE : doesn't work with the constraints***
let serialize( data ) =
	(*** WARNING: mis "No_sharing" completement au pif ***)
	Marshal.to_string data [(*No_sharing; *)Closures]


let unserialize( str ) =
	Marshal.from_string str 0*)



(* Sends a result (first the size then the constraint), by the slave *)
let send_result (*linear_constraint*)im_result =
	let rank = rank() in

	print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] Entering send_constraint");
	let mlc = (*LinearConstraint.serialize_linear_constraint linear_constraint *) serialize_im_result im_result in
	let res_size = String.length mlc in

	print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] Serialized constraint '" ^ mlc ^ "'");
	
	(* Send the result: 1st send the data size, then the data *)
	print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] About to send the size (" ^ (string_of_int res_size) ^ ") of the constraint.");
	Mpi.send res_size masterrank (int_of_slave_tag Slave_result_tag) Mpi.comm_world;
	print_message Debug_high ("[Worker " ^ (string_of_int rank) ^ "] About to send a constraint.");
	Mpi.send mlc masterrank (int_of_slave_tag Slave_result_tag) Mpi.comm_world ;
	print_message Debug_low ("[Worker " ^ (string_of_int rank) ^ "] Sent constraint '" ^ mlc ^ "'");
	()


(* Sends a point (first the size then the point), by the master *)
let send_pi0 pi0 slave_rank =
	let mpi0 = serialize_pi0 pi0 in
	let res_size = String.length mpi0 in
	
	(* Send the result: 1st send the data size, then the data *)
	Mpi.send res_size slave_rank (int_of_master_tag Master_data_tag) Mpi.comm_world;
	Mpi.send mpi0 slave_rank (int_of_master_tag Master_data_tag) Mpi.comm_world
(* 		Printf.printf "[%d] result %s sent" (rank()) result ;  *)
(* 	print_newline() *)


let send_work_request () =
	Mpi.send (weird_stuff()) masterrank (int_of_slave_tag Slave_work_tag) Mpi.comm_world


let receive_pull_request () =
	print_message Debug_medium ("Entered function 'receive_pull_request'...");
	
	(* First receive the length of the data we are about to receive *)
    let (len, source_rank, tag) = 
		Mpi.receive_status Mpi.any_source Mpi.any_tag Mpi.comm_world
	in
	
	print_message Debug_medium ("MPI status received from " ^ ( string_of_int source_rank));
	let tag = slave_tag_of_int tag in
	print_message Debug_medium ("Tag decoded.");

	(* Is this a result or a simple pull ? *)
	match tag with
	| Slave_result_tag ->
		print_message Debug_medium ("[Master] Received Slave_result_tag from " ^ ( string_of_int source_rank) );
		(* receive the result itself *)
(*		let buff = String.create len in
		let res = ref buff in*)
		print_message Debug_medium ("[Master] Buffer created with length " ^ (string_of_int len));
		let res = Mpi.receive source_rank (int_of_slave_tag Slave_result_tag) Mpi.comm_world in
		print_message Debug_medium ("[Master] Reception done");
		print_message Debug_medium ("[Master] Reception done (oui oui)");
		let l = String.length res in
		print_message Debug_medium ("[Master] Calcul taille");
		print_int l;
		print_message Debug_medium ("[Master] Taille affichee");
		
		print_char res.[0];
		print_message Debug_medium ("[Master] Coucou j'ai ecrit le premier caractere !");

		(* Print some information *)
		if debug_mode_greater Debug_medium then
			print_message Debug_medium ("[Master] Tag was '" ^ (*!*)res ^ "'");
			
		(* Get the constraint *)
		let im_result = unserialize_im_result (*!*)res in
		PullAndResult (source_rank , im_result)
		
	(* Case error *)
	| Slave_outofbound_tag ->
		print_message Debug_medium ("[Master] Received Slave_outofbound_tag");
		OutOfBound source_rank
	
	(* Case simple pull? *)
	| Slave_work_tag ->
		print_message Debug_medium ("[Master] Received Slave_work_tag");
		PullOnly source_rank


let send_finished source_rank = 
	Mpi.send (weird_stuff()) source_rank (int_of_master_tag Master_finished_tag) Mpi.comm_world 

let receive_work () =
	(* Get the model *)
	let model = Input.get_model() in

	let ( w, _, tag ) =
	Mpi.receive_status masterrank Mpi.any_tag Mpi.comm_world in

	let tag = master_tag_of_int tag in

	match tag with
	| Master_data_tag -> 
		(* Receive the data itself *)
		let buff = String.create w in
		let work = ref buff in

		work := Mpi.receive masterrank (int_of_master_tag Master_data_tag) Mpi.comm_world;
		
		print_message Debug_high ("Received " ^ (string_of_int w) ^ " bytes of work '" ^ !work ^ "' with tag " ^ (string_of_int (int_of_master_tag Master_data_tag)));
		
		(* Get the pi0 *)
		let pi0 = unserialize_pi0 !work in
		(*** HACK ***)
		(* Convert back to an array *)
		let array_pi0 = Array.make model.nb_parameters NumConst.zero in
		List.iter (fun (variable_index, variable_value) ->
			array_pi0.(variable_index) <- variable_value;
		) pi0;
		(* Convert the pi0 to functional representation *)
		let pi0_fun = fun parameter -> array_pi0.(parameter) in
		Work pi0_fun

	| Master_finished_tag -> Stop

