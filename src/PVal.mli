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
 * Last modified     : 2019/08/09
 *
 ************************************************************)


 (** Global function: set the number of dimensions for ALL parameter valuations; must be called (once and only once) before creating any object *)
val set_dimensions : int -> unit

(** Global function: get the number of dimensions for ALL parameter valuations *)
val get_dimensions : unit -> int


class pval :
	object
		
		(** Get the value for a dimension *)
		method get_value : int -> NumConst.t

		(** Set the value for a dimension *)
		method set_value : int -> NumConst.t -> unit

		(** Copy a parameter valuation *)
		method copy : pval

end

(** Create a pval from a valuation function *)
(*** NOTE: no check is made that the valuation_function is indeed defined for all parameters! If not, an exception may be raised depending on how valuation_function is implemented ***)
val pval_from_valuation_function : (int -> NumConst.t) -> pval
