(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre
 * 
 * Created:       2014/09/24
 * Last modified: 2014/09/24
 *
 ****************************************************************)

(** Global function: set the number of dimensions for ALL hyper rectangles; must be called (once and only once) before creating any object *)
val set_dimensions : int -> unit



class hyper_rectangle :
	object
		
		(** Get the minimum value for a dimension *)
		method get_min : int -> NumConst.t
		(** Get the maximum value for a dimension *)
		method get_max : int -> NumConst.t

		(** Set the minimum value for a dimension *)
		method set_min : int -> NumConst.t -> unit
		(** Set the maximum value for a dimension *)
		method set_max : int -> NumConst.t -> unit

		(** Get the smallest point in the hyper rectangle (i.e., the list of min) in the form of a list of n values *)
		method get_smallest_point : unit -> NumConst.t list
end
