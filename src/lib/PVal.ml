(************************************************************
 *
 *                       IMITATOR
 *
 * Université Paris 13, LIPN, CNRS, France
 *
 * Module description: Class for parameter valuation
 *
 * File contributors : Étienne André
 * Created           : 2014/10/01
 *
 ************************************************************)
 
 
(****************************************************************)
(* Modules *)
(****************************************************************)
open Exceptions
 

(****************************************************************)
(* Handling the global number of dimensions *)
(****************************************************************)
(* Number of dimensions (singleton pattern) *)
let nb_dim = ref None

(* All dimensions, i.e., [0, …, nb_dim] (singleton pattern) *)
let all_dimensions = ref None


(** Set the number of dimensions for ALL parameter valuations; must be called (once and only once) before creating any object *)
let set_dimensions nb_dimensions =
	begin
	match !nb_dim with
	| None ->
		(* Set number of dimensions *)
		nb_dim := Some nb_dimensions;
		(* Set list of all dimensions *)
		all_dimensions := Some (OCamlUtilities.list_of_interval 0 (nb_dimensions - 1));
	| _ -> raise (InternalError "Trying to set the number of dimensions of PVal although it was already set before.")
	end;
	()

let get_dim () =
	begin
	match !nb_dim with
	| None -> raise (InternalError "Trying to access PVal although the number of dimensions was not set before.")
	| Some nb_dim -> nb_dim
	end

let get_all_dimensions () =
	begin
	match !all_dimensions with
	| None -> raise (InternalError "Trying to access PVal although the number of dimensions was not set before.")
	| Some all_dimensions -> all_dimensions
	end


let assert_nb_dim_initialized () =
	begin
	match !nb_dim with
	| None -> raise (InternalError "Trying to access PVal although the number of dimensions was not set before.")
	| _ -> ()
	end


let assert_dim_valid dim =
	let nb_dim = get_dim() in
	if dim >= nb_dim then
		raise (InternalError ("Trying to access dimension " ^ (string_of_int dim) ^ " in a PVal although the number of dimensions is " ^ (string_of_int nb_dim) ^ "."))


(** Get the number of dimensions for ALL parameter valuations; must be called (once and only once) before creating any object *)
let get_dimensions = get_dim


(****************************************************************)
class pval =
(****************************************************************)
	object
		val mutable the_array =
			assert_nb_dim_initialized ();
			(* Initialize to 0 *)
			Array.make (get_dim()) NumConst.zero
		
		(** Get the value for a dimension *)
		method get_value dim =
			(* First check that the number of dimensions has been set *)
			assert_nb_dim_initialized();
			(* Then check that the dimension is valid *)
			assert_dim_valid dim;
			(* Get the value *)
			the_array.(dim)
		
		(** Set the value for a dimension *)
		method set_value dim value =
			(* First check that the number of dimensions has been set *)
			assert_nb_dim_initialized();
			(* Then check that the dimension is valid *)
			assert_dim_valid dim;
			(* Set the min *)
			the_array.(dim) <- value
			
		(** Copy a parameter valuation *)
		method copy =
			(* First check that the number of dimensions has been set *)
			assert_nb_dim_initialized();
			(* Create second object *)
			let pval_copy = new pval in
			(* Copy one by one *)
			for dim = 0 to get_dim() -1 do
				pval_copy#set_value dim the_array.(dim);
			done;
			(* Return *)
			pval_copy

	;
end



(** Create a pval from a valuation function *)
(*** NOTE: no check is made that the valuation_function is indeed defined for all parameters! If not, an exception may be raised depending on how valuation_function is implemented ***)
let pval_from_valuation_function (valuation_function : (int -> NumConst.t)) : pval =
	(* Get dimensions in the form of a list *)
	let all_dimensions = get_all_dimensions () in
	
	(* Create fresh valuation *)
	let pval = new pval in

	(* Update it *)
	List.iter (fun parameter ->
		pval#set_value parameter (valuation_function parameter);
	) all_dimensions;
	
	(* Return it *)
	pval
