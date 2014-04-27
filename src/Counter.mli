(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre
 * 
 * Created:       2014/04/27
 * Last modified: 2014/04/27
 *
 ****************************************************************)
 
class counter :
	object
		
		(** Initialize the counter *)
		method init : unit

		(** Start the counter *)
		method start : unit
			
		(** Stop the counter *)
		method stop : unit
			
		(** Reset the counter *)
		method reset : unit

		(** Get the counter's value *)
		method value : float
end