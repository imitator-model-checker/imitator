(************************************************************
 *
 *                     IMITATOR II
 *
 * Represent a reachability tree for depth first exploration
 *
 * LIPN, Universite Paris 13, Sorbonne Paris Cite, France
 * Author:        Etienne Andre
 * Created:       2012/06/07
 * Last modified: 2012/06/07
 *
 ************************************************************)

 
(************************************************************)
(* Modules *)
(************************************************************)
open Global


(************************************************************)
(* Type *)
(************************************************************)

type 'a rtree = {
	(* Table element --> children *)
	children : ('a , 'a list) Hashtbl.t;
	(* Table element --> parent *)
	parent : ('a , 'a ) Hashtbl.t;
	(* Table element --> visited *)
	visited : ('a , bool) Hashtbl.t;
}

(************************************************************)
(* Exceptions *)
(************************************************************)

exception ElementBound


(************************************************************)
(* Functions *)
(************************************************************)

(** Create a new tree with initial (guess) size for the number of elements *)
let create initial_size =
	{
		children = Hashtbl.create initial_size;
		parent = Hashtbl.create initial_size;
		visited = Hashtbl.create initial_size;
	}


(** Get the parent of an element; raise a Not_found exception if the element is root *)
let get_parent element rtree =
	Hashtbl.find rtree.parent element

(** Get the (possibly empty) list of children of an element *)
let get_children element rtree =
	try(
		Hashtbl.find rtree.children element
	) with
		Not_found -> []


(** Add a list of children to an element; raise an ElementBound exception if children are defined already *)
let set_children element children rtree =
	(* Check if element is already set *)
	(** TO OPTIMIZE: might use time *)
	if Hashtbl.mem rtree.children element then (raise ElementBound);
	(* Add *)
	Hashtbl.add rtree.children element children


(** Set the (common) parent of a list of elements; raise an ElementBound exception if an element already has a parent *)
let set_parent children parent rtree =
	List.iter(fun child ->
		(* Check if element is already set *)
		(** TO OPTIMIZE: might use time *)
		if Hashtbl.mem rtree.parent child then (raise ElementBound);
		(* Add *)
		Hashtbl.add rtree.parent child parent
	) children


(** Add a list of children to a node; raise an ElementBound exception if children are defined already *)
let add_children parent children rtree =
	(* Set the children *)
	set_children parent children rtree;
	(* Set the parent *)
	set_parent children parent rtree


(** Convert a rtree to a string (for debug printing) *)
let string_of_rtree string_of_element rtree =
	"CHILDREN"
	^ (Hashtbl.fold (fun element children current_string -> current_string ^ "\n  " ^ (string_of_element element) ^ ": "
		^ (string_of_list_of_string_with_sep ", " (List.map string_of_element children))
	) rtree.children "")
	^ "\nPARENTS"
	^ (Hashtbl.fold (fun element parent current_string ->
			current_string ^ "\n  " ^ (string_of_element element) ^ ": "
			^ (string_of_element parent)
		) rtree.parent "")
	^ "\nVISITED"
	^ (Hashtbl.fold (fun element visited current_string ->
			current_string ^ "\n  " ^ (string_of_element element) ^ ": "
			^ (string_of_bool visited)
		) rtree.visited "")
		


let test () =
	(** TESTS FOR REACHABILITY TREE *)
	let rt = create 10 in
	add_children 0 [1; 7; 5] rt;
	print_newline();
	print_string (string_of_rtree string_of_int rt);
	(*add_children 0 [2; 8] rt;
	print_string (string_of_rtree string_of_int rt);*)

	print_newline();
	print_string ("Parent of 7: " ^ (string_of_int (get_parent 7 rt)));
	print_newline();
	print_string ("Children of 0: " ^ (string_of_list_of_string_with_sep "," (List.map string_of_int (get_children 0 rt))));
	print_newline();
	print_string ("Children of 237: " ^ (string_of_list_of_string_with_sep "," (List.map string_of_int (get_children 237 rt))));
	(*print_newline();
	print_string ("Parent of 1999: " ^ (string_of_int (get_parent 1999 rt)));*)
