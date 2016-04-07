(************************************************************
 *
 *                     IMITATOR II
 *
 * Represent a reachability tree for depth first exploration
 *
 * LIPN, Universite Paris 13, Sorbonne Paris Cite, France
 * Author:        Etienne Andre
 * Created:       2012/06/07
 * Last modified: 2015/03/30
 *
 ************************************************************)

 
(************************************************************)
(* Modules *)
(************************************************************)
open Exceptions
open OCamlUtilities
open ImitatorUtilities


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
	(* Recall the root (could be retrieved from parent, but fastens the analysis) *)
	root : 'a;
	(* Recall the number of states to visit (can be retrieved from visited, but fastens the analysis) *)
	mutable nb_nonvisited : int;
	(* The current element for the exploration (initially root) *)
	mutable current_element : 'a;
}

(************************************************************)
(* Exceptions *)
(************************************************************)

exception ElementBound


(************************************************************)
(* Creation function *)
(************************************************************)

(** Create a new tree with initial (guess) size for the number of elements *)
let create initial_size root =
	let visited = Hashtbl.create initial_size in
	(* Add the root *)
	Hashtbl.add visited root false;
	(* Return the structure *)
	{
		children = Hashtbl.create initial_size;
		parent = Hashtbl.create initial_size;
		visited = visited;
		root = root;
		nb_nonvisited = 1;
		current_element = root;
	}


(************************************************************)
(* GET function *)
(************************************************************)

(** Get the parent of an element; raise a Not_found exception if the element is root *)
let get_parent element rtree =
	Hashtbl.find rtree.parent element

(** Get the (possibly empty) list of children of an element *)
let get_children element rtree =
	try(
		Hashtbl.find rtree.children element
	) with
		Not_found -> []


(** Return the number of states that are not yet visited *)
let nb_states_to_visit rtree =
	rtree.nb_nonvisited


(************************************************************)
(* SET function *)
(************************************************************)

exception FoundNext

(** Get (and update) the next element to visit according to a depth-first exploration; raise Not_found if all states are visited *)
(* Assume that:
	1) a non visited child has no children
	2) a parent is always visited *)
let get_next_element rtree =
	(* Badprog : could not set a FoundNext of 'a *)
	let next = ref rtree.current_element in
	(* Recursive function for checking an element ; raise Not_found if root is reached *)
	let rec check_element element = 
		print_message Verbose_total ("\nChecking a new element");
		(* First check the children *)
		let children = get_children element rtree in
		(* Check if each children is visited *)
		List.iter (fun child ->
			let child_is_visited = try (Hashtbl.find rtree.visited child) with
				Not_found -> raise (InternalError ("Error in get_next_element ; the child of some element has no entry at all in the visited table."));
			in
			if not child_is_visited then (
				print_message Verbose_total ("Child not visited! Next is found.");
				next := child;
				raise FoundNext
			) else (print_message Verbose_total ("Child visited."););
		) children;
		
		print_message Verbose_total ("Children all visited.");
		(* Then check higher in the hierarchy, except if root *)
		if element != rtree.root then(
			print_message Verbose_total ("Element is not root: go to parent.");
			(* Retrieve the parent *)
			let parent = Hashtbl.find rtree.parent element in
			(* Check the parent *)
			check_element parent
		)
		(* If root : raise Not_found *)
		else (
			print_message Verbose_total ("Element is root: not found!");
			raise Not_found
		);
	in	
	(* Global exception mechanism when the child is found *)
	try(
		print_message Verbose_total ("Starting to look for the new element.");
		check_element rtree.current_element
	) with FoundNext ->
		(* Set the new element *)
		rtree.current_element <- !next;
		(* Return it *)
		!next


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


(** Set a list of elements to be non visited; raise an ElementBound exception if an element already is in the visited table *)
let set_nonvisited elements rtree =
	List.iter(fun element ->
		(* Check if element is already set *)
		(** TO OPTIMIZE: might use time *)
		if Hashtbl.mem rtree.visited element then (raise ElementBound);
		(* Add *)
		Hashtbl.add rtree.visited element false
	) elements;
	(* Update the number of states to visit *)
	rtree.nb_nonvisited <- rtree.nb_nonvisited + (List.length elements)


(** Set an element to be visited; raise an Not_found exception if the element does not exist in the table *)
let set_visited element rtree =
	(* Check if element is already set *)
	(** TO OPTIMIZE: might use time *)
	if not (Hashtbl.mem rtree.visited element) then (raise Not_found);
	(* Add *)
	Hashtbl.replace rtree.visited element true;
	(* Update the number of states to visit *)
	rtree.nb_nonvisited <- rtree.nb_nonvisited - 1


(** Add a list of children to a node; raise an ElementBound exception if children are defined already *)
let add_children parent children rtree =
	(* Set the children *)
	set_children parent children rtree;
	(* Set the parent *)
	set_parent children parent rtree;
	(* Set non visited *)
	set_nonvisited children rtree



(************************************************************)
(* String function *)
(************************************************************)

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
	^ "\nNUMBERS OF STATES TO VISIT: "
	^ (string_of_int rtree.nb_nonvisited)
	^ "\nCURRENT STATE: "
	^ (string_of_element rtree.current_element)
		


(************************************************************)
(* Tests function *)
(************************************************************)

let test () =
	(** TESTS FOR REACHABILITY TREE *)
	let rt = create 10 0 in
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
