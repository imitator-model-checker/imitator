(************************************************************
 *
 *                     IMITATOR II
 *
 * Represent a reachability tree for depth first exploration
 *
 * LIPN, Universite Paris 13, Sorbonne Paris Cite, France
 * Author:        Etienne Andre
 * Created:       2012/06/07
 * Last modified: 2012/06/21
 *
 ************************************************************)

 
(************************************************************)
(* Type *)
(************************************************************)

type 'a rtree

(************************************************************)
(* Exception *)
(************************************************************)

exception ElementBound


(************************************************************)
(* Functions *)
(************************************************************)


(** Create a new tree with initial (guess) size for the number of elements, and a root element *)
val create : int -> 'a -> 'a rtree


(** Get the parent of an element; raise a Not_found exception if the element is root *)
val get_parent : 'a -> 'a rtree -> 'a

(** Get the (possibly empty) list of children of an element *)
val get_children : 'a -> 'a rtree -> 'a list


(** Return the number of states that are not yet visited *)
val nb_states_to_visit : 'a rtree -> int


(** Get (and update) the next element to visit according to a depth-first exploration; raise Not_found if all states are visited *)
val get_next_element : 'a rtree -> 'a


(*(** Add a list of children to an element; raise an ElementBound exception if children are defined already *)
val set_children : 'a -> 'a list -> 'a rtree -> unit

(** Set the (common) parent of a list of elements; raise an ElementBound exception if an element already has a parent *)
val set_parent : 'a list -> 'a -> 'a rtree -> unit*)

(** Set an element to be visited; raise an Not_found exception if the element does not exist in the table *)
val set_visited : 'a -> 'a rtree -> unit

(** Add a list of children to a node; raise an ElementBound exception if children are defined already *)
val add_children : 'a -> 'a list -> 'a rtree -> unit


(** Convert a rtree to a string (for debug printing) *)
val string_of_rtree : ('a -> string) -> 'a rtree -> string

