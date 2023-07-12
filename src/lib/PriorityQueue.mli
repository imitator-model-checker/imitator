(************************************************************
 *
 *                       IMITATOR
 * 
 * Aarhus University
 * 
 * Module description: Priority Queue (Smallest element has highest priority!)
*                      Implemented using a Leftist Heap
 * 
 * File contributors : Mikael Bisgaard Dahlsen-Jensen
 * Created           : 2023/07/12
 *
 ************************************************************)

type comparison = Equal | GreaterThan | LessThan

module type OrderedSig =
  sig
    type t
    val compare: t -> t -> comparison
  end

module type PriorityQueueSig =
	sig
    exception Queue_is_empty
    type t
    type queue
    type rank
    val empty : queue

    (* Time complexity: O (log n) *)
	  val merge : queue -> queue -> queue

  	(* Time complexity: O (1) *)
	  val is_empty : queue  -> bool

  	(* Time complexity: O (log n) *)
	  val insert : queue  -> t -> queue

	  (* Time complexity: O (log n) *)
	  val extract : queue -> t * queue

	  (* Time complexity: O (1) *)
    val peek : queue -> t

	  (* Time complexity: O (1) *)
    val length : queue -> int

	  (* Time complexity: O (n) *)
    val to_list : queue -> t list

	  (* Time complexity: O (n) *)
    val of_list : t list -> queue
	end

module Make (Elt : OrderedSig) : (PriorityQueueSig with type t = Elt.t)
