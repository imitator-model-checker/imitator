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
		method copy : unit -> pval

end
