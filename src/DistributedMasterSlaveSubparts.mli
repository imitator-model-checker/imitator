(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre, Camille Coti, Hoang Gia Nguyen
 * 
 * Created:       2014/09/05
 * Last modified: 2015/04/12
 *
 ****************************************************************)


(** Implementation of master *)
val master : unit -> unit

(** Implementation of slave *)
val worker : unit -> unit

(** Implementation of coordinator (for static distribution);
	WARNING: only works with a number of nodes equal to a power of 2 *)
val collaborator : unit -> unit

