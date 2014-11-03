(*****************************************************************
 *
 *                       IMITATOR
 * 
 *
 * Description:   Class for parameter valuation
 * 
 * Author:        Etienne Andre
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Created:       2014/10/01
 * Last modified: 2014/10/02
 *
 ****************************************************************)

 
(****************************************************************)
(* Modules *)
(****************************************************************)
open Exceptions
 

(****************************************************************)
(* Handling the global number of dimensions *)
(****************************************************************)
(* Singleton pattern *)
let nb_dim = ref None


(** Set the number of dimensions for ALL parameter valuations; must be called (once and only once) before creating any object *)
let set_dimensions nb_dimensions =
	begin
	match !nb_dim with
	| None -> nb_dim := Some nb_dimensions
	| Some _ -> raise (InternalError "Trying to set the number of dimensions of PVal although it was already set before.")
	end;
	()

let get_dim () =
	begin
	match !nb_dim with
	| None -> raise (InternalError "Trying to access PVal although the number of dimensions was not set before.")
	| Some nb_dim -> nb_dim
	end


let assert_nb_dim_initialized () =
	begin
	match !nb_dim with
	| None -> raise (InternalError "Trying to access PVal although the number of dimensions was not set before.")
	| Some _ -> ()
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
		method copy () =
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


