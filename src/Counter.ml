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


class counter =
	object
		val mutable value = ref 0.0
		val mutable start_time = ref 0.0
		
		(** Initialize the counter *)
		method init =
			value := 0.0 ;
			start_time := Unix.gettimeofday()
			
		(** Start the counter *)
		method start =
			start_time := Unix.gettimeofday()
			
		(** Stop the counter *)
		method stop =
			value := !value +. Unix.gettimeofday() -. !start_time
			
		(** Reset the counter *)
		method reset = 
			value := 0.0

		(** Get the counter's value *)
		method value =
			!value
			
end
