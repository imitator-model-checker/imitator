(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre
 * 
 * Created:       2014/09/24
 * Last modified: 2014/10/01
 *
 ****************************************************************)

(****************************************************************)
(* Modules *)
(****************************************************************)
open Global
 

(****************************************************************)
(* Handling the global number of dimensions *)
(****************************************************************)
(* Singleton pattern *)
let nb_dim = ref None


(** Set the number of dimensions for ALL hyper rectangles; must be called (once and only once) before creating any object *)
let set_dimensions nb_dimensions =
	begin
	match !nb_dim with
	| None -> nb_dim := Some nb_dimensions
	| Some _ -> raise (InternalError "Trying to set the number of dimensions of HyperRectangle although it was already set before.")
	end;
	()

let get_dim () =
	begin
	match !nb_dim with
	| None -> raise (InternalError "Trying to access HyperRectangle although the number of dimensions was not set before.")
	| Some nb_dim -> nb_dim
	end


let assert_nb_dim_initialized () =
	begin
	match !nb_dim with
	| None -> raise (InternalError "Trying to access HyperRectangle although the number of dimensions was not set before.")
	| Some _ -> ()
	end


let assert_dim_valid dim =
	let nb_dim = get_dim() in
	if dim >= nb_dim then
		raise (InternalError ("Trying to access dimension " ^ (string_of_int dim) ^ " in a HyperRectangle although the number of dimensions is " ^ (string_of_int nb_dim) ^ "."))


(** Get the number of dimensions for ALL hyper rectangles; must be called (once and only once) before creating any object *)
let get_dimensions = get_dim


(****************************************************************)
class hyper_rectangle =
(****************************************************************)
	object
		val mutable the_array =
			assert_nb_dim_initialized ();
			(* Initialize to pairs (0,0) *)
			Array.make (get_dim()) (NumConst.zero, NumConst.zero)
		
		(** Get the minimum value for a dimension *)
		method get_min dim =
			(* First check that the number of dimensions has been set *)
			assert_nb_dim_initialized();
			(* Then check that the dimension is valid *)
			assert_dim_valid dim;
			(* Get the min *)
			let (min, _) = the_array.(dim) in
			min
		
		(** Get the maximum value for a dimension *)
		method get_max dim =
			(* First check that the number of dimensions has been set *)
			assert_nb_dim_initialized();
			(* Then check that the dimension is valid *)
			assert_dim_valid dim;
			(* Get the max *)
			let (_, max) = the_array.(dim) in
			max

		(** Set the minimum value for a dimension *)
		method set_min dim value =
			(* First check that the number of dimensions has been set *)
			assert_nb_dim_initialized();
			(* Then check that the dimension is valid *)
			assert_dim_valid dim;
			(* Set the min *)
			let (_, max) = the_array.(dim) in
			the_array.(dim) <- (value, max)

			
		(** Set the maximum value for a dimension *)
		method set_max dim value =
			(* First check that the number of dimensions has been set *)
			assert_nb_dim_initialized();
			(* Then check that the dimension is valid *)
			assert_dim_valid dim;
			(* Set the max *)
			let (min, _) = the_array.(dim) in
			the_array.(dim) <- (min, value)

(*		(** Get the smallest point in the hyper rectangle (i.e., the list of min) in the form of a list of n values *)
 		method get_smallest_point () =
			(* First check that the number of dimensions has been set *)
			assert_nb_dim_initialized();
			(* Convert using predefined functions *)
			let first, _ = List.split (Array.to_list(the_array)) in
			first*)
			
		(** Get the smallest point in the hyper rectangle (i.e., the list of min) in the form of a PVal.pval *)
 		method get_smallest_point () =
			(* First check that the number of dimensions has been set *)
			assert_nb_dim_initialized();
			(* Create parameter valuation *)
			let pval = new PVal.pval in
			(* Assign for all dimensions *)
			let nb_dim = get_dim () in
			for dim = 0 to nb_dim - 1 do
				(* Get the minimum value *)
				let min, _ = the_array.(dim) in
				(* Assign *)
				pval#set_value dim min;
			done;
			(* Return *)
			pval
	;
end


