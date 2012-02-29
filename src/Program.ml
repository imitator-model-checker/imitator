open Global
open Graph
open AbstractImitatorFile

(* internal references to global data structures *)
let program_ref = ref None
let pi0_ref = ref None
let pi0cube_ref = ref None
let options_ref = ref None
let reachability_graph_ref = ref None
let abstract_reachability_graph_ref = ref None


let get_program _ =
	match !program_ref with
		| None ->
			raise (InternalError "Input program not available");
		| Some program -> program

let set_program program =
	program_ref := Some program

let get_pi0 _ =
	match !pi0_ref with
		| None ->
			raise (InternalError "Input pi0 not available");
		| Some pi0 -> pi0

let set_pi0 pi0 =
	pi0_ref := Some pi0

let get_pi0cube _ =
	match !pi0cube_ref with
		| None ->
			raise (InternalError "Input pi0cube not available");
		| Some pi0cube -> pi0cube

let set_pi0cube pi0cube =
	pi0cube_ref := Some pi0cube

let get_options _ =
	match !options_ref with
		| None ->
			raise (InternalError "Options not available");
		| Some options -> options

let set_options options =
	options_ref := Some options
				 
let get_reachability_graph _ =
	match !reachability_graph_ref with
		| None ->
			raise (InternalError "Reachability graph not available");
		| Some reachability_graph -> reachability_graph

let set_reachability_graph reachability_graph =
	reachability_graph_ref := Some reachability_graph
				 
let get_abstract_reachability_graph _ =
	match !abstract_reachability_graph_ref with
		| None ->
			raise (InternalError "Abstract reachability graph not available");
		| Some abstract_reachability_graph -> abstract_reachability_graph

let set_abstract_reachability_graph abstract_reachability_graph =
	abstract_reachability_graph_ref := Some abstract_reachability_graph
				 
	