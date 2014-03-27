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
 * Last modified: 2014/03/25
 *
 ****************************************************************)

open Global
open Mpi
open Marshal


(****************************************************************)
(** Public types *)
(****************************************************************)
type rank = int

type pull_request =
	| PullOnly of rank
	| PullAndResult of rank * LinearConstraint.p_linear_constraint
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
(* Returns a list of substrings splitted using sep *)
let split sep = Str.split (Str.regexp ("[" ^ sep ^ "]"))

let serialize_numconst = NumConst.string_of_numconst
let unserialize_numconst = NumConst.numconst_of_string

(* Separator between the two elements of a pair *)
let serialize_SEP_PAIR = ","
(* Separator between the elements of a list *)
let serialize_SEP_LIST = ";"

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
(*	| [] -> ""
	| [ pair ] -> serialize_pi0_pair pair
	| pair :: rest ->
		serialize_pi0_pair pair
		^ serialize_SEP_LIST
		^ serialize_pi0 rest*)

let unserialize_pi0_pair pi0_pair_string =
	match split serialize_SEP_PAIR pi0_pair_string with
	| [variable_string ; value_string ] ->
		LinearConstraint.unserialize_variable variable_string , unserialize_numconst value_string
	| _ -> raise (SerializationError ("Cannot unserialize pi0 value '" ^ pi0_pair_string ^ "': (variable_index, value) expected."))


let unserialize_pi0 pi0_string =
	let pi0_pairs_string = split serialize_SEP_LIST pi0_string in
	List.map unserialize_pi0_pair pi0_pairs_string


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
(*** WARNING: le "ref 1" ne signifie rien du tout ***)
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
	| _ -> raise (InternalError ("Impossible match in slave_tag_of_int."))

let master_tag_of_int = function
	| 17 -> Master_data_tag 
	| 18 -> Master_finished_tag
	| _ -> raise (InternalError ("Impossible match in master_tag_of_int."))


let size () = Mpi.comm_size Mpi.comm_world
let rank () = Mpi.comm_rank Mpi.comm_world



let serialize( data ) =
	(*** WARNING: mis "No_sharing" completement au pif ***)
	Marshal.to_string data [(*No_sharing; *)Closures]


let unserialize( str ) =
	Marshal.from_string str 0



(* Sends a constraint (first the size then the constraint), by the slave *)
let send_constraint linear_constraint =
	let rank = rank() in

	print_message Debug_low ("Worker " ^ (string_of_int rank) ^ " starts send_constraint");
	let mlc = serialize(linear_constraint)  in
	let res_size = String.length mlc in
	
	(* Send the result: 1st send the data size, then the data *)
	print_message Debug_low ("Worker " ^ (string_of_int rank) ^ " is about to send the size (" ^ (string_of_int res_size) ^ ") of the constraint.");
	Mpi.send res_size masterrank (int_of_slave_tag Slave_result_tag) Mpi.comm_world;
	print_message Debug_low ("Worker " ^ (string_of_int rank) ^ " is about to send the constraint.");
	Mpi.send mlc masterrank (int_of_slave_tag Slave_result_tag) Mpi.comm_world
(* 		Printf.printf "[%d] result %s sent" (rank()) result ;  *)
(* 	print_newline() *)


(* Sends a point (first the size then the point), by the master *)
let send_pi0 pi0 slave_rank =
	let mpi0 = serialize(pi0)  in
	let res_size = String.length mpi0 in
	
	(* Send the result: 1st send the data size, then the data *)
	Mpi.send res_size slave_rank (int_of_master_tag Master_data_tag) Mpi.comm_world;
	Mpi.send mpi0 slave_rank (int_of_master_tag Master_data_tag) Mpi.comm_world
(* 		Printf.printf "[%d] result %s sent" (rank()) result ;  *)
(* 	print_newline() *)


let send_work_request () =
	Mpi.send (weird_stuff()) masterrank (int_of_slave_tag Slave_work_tag) Mpi.comm_world


let receive_pull_request () =
	(* First receive the length of the data we are about to receive *)
      let (len, source_rank, tag) = 
	Mpi.receive_status Mpi.any_source Mpi.any_tag Mpi.comm_world in
	
	let tag = slave_tag_of_int tag in

	(* Is this a result or a simple pull ? *)
	match tag with
	| Slave_result_tag ->
		(* receive the result itself *)
		let buff = String.create len in
		let res = ref buff in
		res := Mpi.receive source_rank (int_of_slave_tag Slave_result_tag) Mpi.comm_world;
			
		(* Get the constraint *)
		let linear_constraint = unserialize !res in
		PullAndResult (source_rank , linear_constraint)
		
	(* Case error *)
	| Slave_outofbound_tag -> OutOfBound source_rank
	
	(* Case simple pull? *)
	| Slave_work_tag -> PullOnly source_rank


let send_finished source_rank = 
	Mpi.send (weird_stuff()) source_rank (int_of_master_tag Master_finished_tag) Mpi.comm_world 

let receive_work () =
	let ( w, _, tag ) =
	Mpi.receive_status masterrank Mpi.any_tag Mpi.comm_world in

	let tag = master_tag_of_int tag in

	match tag with
	| Master_data_tag -> 
		(* Receive the data itself *)
		let buff = String.create w in
		let work = ref buff in

		work := Mpi.receive masterrank (int_of_master_tag Master_data_tag) Mpi.comm_world; 
		Printf.printf "recv %d bytes of work %s with tag %d" w !work (int_of_master_tag Master_data_tag) ;
		print_newline() ;
		
		(* Get the constraint *)
		let pi0 = unserialize !work in
		Work pi0

	| Master_finished_tag -> Stop

