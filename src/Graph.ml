(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/12/08
 * Last modified: 2010/03/29
 *
 ****************************************************************)


open Global
open AbstractImitatorFile

(****************************************************************)
(** Graph structure *)
(****************************************************************)
type state_index = int

type reachability_graph = {
(*	(** A hashtable 'state' -> 'state_index' *)
	states : (AbstractImitatorFile.state, state_index) Hashtbl.t;
*)
	(** An Array 'state_index' -> 'state' *)
	states : AbstractImitatorFile.state DynArray.t;

	(** A hashtable '(state_index, action_index)' -> 'dest_state_index' *)
	transitions_table : ((state_index * AbstractImitatorFile.action_index), state_index) Hashtbl.t;
	
	(** A constraint shared by all states *)
(*	mutable shared_constraint : LinearConstraint.linear_constraint;*)
}


(****************************************************************)
(** Constant *)
(****************************************************************)
(** Initial size of the array of states (will be updated automatically *)
let initial_size = 100


(****************************************************************)
(** Graph creation *)
(****************************************************************)

(** Create a fresh graph *)
let make guessed_nb_transitions = 
	(* Create a hashtable for the reachable states *)
	let states = DynArray.make initial_size in
	(* Create a hashtable for the graph *)
	let transitions_table = Hashtbl.create guessed_nb_transitions in
	(* initialize shared constraint with true *)
(*	let shared_constraint = LinearConstraint.true_constraint () in*)
	(* Create the graph *)
	{
		states = states;
		transitions_table = transitions_table;
(*		shared_constraint = shared_constraint;*)
	}


(****************************************************************)
(** Interrogation on a graph *)
(****************************************************************)

(** Return the number of states in a graph *)
let nb_states graph =
	DynArray.length graph.states

(** Return the state of a state_index *)
let get_state graph state_index =
	DynArray.get graph.states state_index 

(** Returns the shared constraint for all states *)
(*let get_shared_constraint graph =*)
(*	graph.shared_constraint        *)

(** Return the list of all constraints on the parameters associated to the states of a graph *)
let all_p_constraints program graph =
	DynArray.fold_left
		(fun current_list (_, linear_constraint) ->
			let p_constraint = LinearConstraint.hide program.clocks_and_discrete linear_constraint in
			p_constraint :: current_list)
		[] graph.states


(** Check if a state belongs to the graph *)
(*let in_graph graph state =
	Hashtbl.mem graph.states state*)


(** Return the state_index of a state; raise Not_found if not found *)
(*let find_state_index graph state =
	Hashtbl.find graph.states state*)


(** Check if two states are equal *)
let states_equal state1 state2 =
	let (loc1, constr1) = state1 in
	let (loc2, constr2) = state2 in
	if not (Automaton.location_equal loc1 loc2) then false else (
		LinearConstraint.is_equal constr1 constr2
	)

(** Iterate over the reachable states *)
let iter f graph =
	DynArray.iter f graph.states

(****************************************************************)
(** Actions on a graph *)
(****************************************************************)
exception Found of state_index

(** Add a state to a graph: return (state_index, added), where state_index is the index of the state, and 'added' is false if the state was already in the graph, true otherwise *)
let add_state program graph new_state = 
	(* IF ACYCLIC MODE : does not test anything *)
	if program.acyclic then (
		(* Since the state does NOT belong to the graph: find the state index *)
		let new_state_index = DynArray.length graph.states in
		(* Add the state to the table *)
		DynArray.add graph.states new_state;
		(* Return state_index, true *)
		new_state_index, true
	) else (
	(* ELSE : *)
		(* First try to find the exact state in the DynArray *)
		try (
			(* If the same state belongs to the graph *)
			let state_index = DynArray.index_of (fun some_state -> states_equal new_state some_state) graph.states in
			(* Return state_index, false *)
			state_index, false
		) with Not_found -> (
			(* Define the equality *)
			let equality =
				if program.inclusion then LinearConstraint.is_leq
				else LinearConstraint.is_equal
			in
			(* Then test the equality of ALL the states found *)			
			let result =
			try(
				(* Consider the locations and the constraint *)
				let new_location, new_constraint = new_state in
				(* add K to new constraint *)
(*				let new_constraint = LinearConstraint.intersection [new_constraint; k] in*)
				(* Iterate on the states *)
				DynArray.iteri (fun state_index (loc, constr) ->
					(* First check the equality on locations *)
					if loc = new_location then (
						(* Now check the equality of constraints *)
						if equality new_constraint constr then (
						(* Then the state is equal *)
						raise (Found state_index))
					);
				) graph.states;
				(* Since the state does NOT belong to the graph: find the state index *)
				let new_state_index = DynArray.length graph.states in
				(* Add the state to the table *)
				DynArray.add graph.states new_state;
				(* Return state_index, true *)
				new_state_index, true
			(* Return the state index *)
			) with Found state_index -> state_index, false in			
			result
		)
	)

(** Add a transition to the graph *)
let add_transition reachability_graph (orig_state_index, action_index, dest_state_index) =
	Hashtbl.add reachability_graph.transitions_table (orig_state_index, action_index) dest_state_index


(** Add an inequality to all the states of the graph *)
let add_inequality_to_states graph inequality =
	let constraint_to_add = LinearConstraint.make [inequality] in
	(* For all state: *)
	for state_index = 0 to (DynArray.length graph.states) - 1 do
		 DynArray.set graph.states state_index (
			let (loc, const) = DynArray.get graph.states state_index in
			(* Perform the intersection *)
			loc, (LinearConstraint.intersection [constraint_to_add; const] )
		)
	done

(*		graph.shared_constraint <- LinearConstraint.intersection[*)
(*		graph.shared_constraint;                                 *)
(*		constraint_to_add                                        *)
(*	]                                                          *)
	

let plot_graph graph = 
	DynArray.fold_left (
		fun s (_, constr) -> s ^ (LinearConstraint.plot_2d constr) ^ "\n"
	) "" graph.states

(****************************************************************)
(** Interaction with dot *)
(****************************************************************)

(*let dot_colors = [

(* Then all the other one by alphabetic order *)
"aliceblue" ; "antiquewhite" ; "antiquewhite1" ; "antiquewhite2" ; "antiquewhite3" ;
"antiquewhite4" ; "aquamarine" ; "aquamarine1" ; "aquamarine2" ; "aquamarine3" ; 
"aquamarine4" ; "azure" ; "azure1" ; "azure2" ; "azure3" ; 
"azure4" ; "beige" ; "bisque" ; "bisque1" ; "bisque2" ; 
"bisque3" ; "bisque4" ; "black" ; "blanchedalmond" ; (*"blue" ;*) 
"blue1" ; "blue2" ; "blue3" ; "blue4" ; "blueviolet" ; 
"brown" ; "brown1" ; "brown2" ; "brown3" ; "brown4" ; 
"burlywood" ; "burlywood1" ; "burlywood2" ; "burlywood3" ; "burlywood4" ; 
"cadetblue" ; "cadetblue1" ; "cadetblue2" ; "cadetblue3" ; "cadetblue4" ; 
"chartreuse" ; "chartreuse1" ; "chartreuse2" ; "chartreuse3" ; "chartreuse4" ; 
"chocolate" ; "chocolate1" ; "chocolate2" ; "chocolate3" ; "chocolate4" ; 
"coral" ; "coral1" ; "coral2" ; "coral3" ; "coral4" ; 
"cornflowerblue" ; "cornsilk" ; "cornsilk1" ; "cornsilk2" ; "cornsilk3" ; 
"cornsilk4" ; "crimson" ; "cyan" ; "cyan1" ; "cyan2" ; 
"cyan3" ; "cyan4" ; "darkgoldenrod" ; "darkgoldenrod1" ; "darkgoldenrod2" ; 
"darkgoldenrod3" ; "darkgoldenrod4" ; "darkgreen" ; "darkkhaki" ; "darkolivegreen" ; 
"darkolivegreen1" ; "darkolivegreen2" ; "darkolivegreen3" ; "darkolivegreen4" ; "darkorange" ; 
"darkorange1" ; "darkorange2" ; "darkorange3" ; "darkorange4" ; "darkorchid" ; 
"darkorchid1" ; "darkorchid2" ; "darkorchid3" ; "darkorchid4" ; "darksalmon" ; 
"darkseagreen" ; "darkseagreen1" ; "darkseagreen2" ; "darkseagreen3" ; "darkseagreen4" ; 
"darkslateblue" ; "darkslategray" ; "darkslategray1" ; "darkslategray2" ; "darkslategray3" ; 
"darkslategray4" ; "darkslategrey" ; "darkturquoise" ; "darkviolet" ; "deeppink" ; 
"deeppink1" ; "deeppink2" ; "deeppink3" ; "deeppink4" ; "deepskyblue" ; 
"deepskyblue1" ; "deepskyblue2" ; "deepskyblue3" ; "deepskyblue4" ; "dimgray" ; 
"dimgrey" ; "dodgerblue" ; "dodgerblue1" ; "dodgerblue2" ; "dodgerblue3" ; 
"dodgerblue4" ; "firebrick" ; "firebrick1" ; "firebrick2" ; "firebrick3" ; 
"firebrick4" ; "floralwhite" ; "forestgreen" ; "gainsboro" ; "ghostwhite" ; 
"   gold   " ; "gold1" ; "gold2" ; "gold3" ; "gold4" ; 
"goldenrod" ; "goldenrod1" ; "goldenrod2" ; "goldenrod3" ; "goldenrod4" ; 
(*"   gray   " ; "gray0" ; "gray1" ; "gray2" ; "gray3" ; 
"gray4" ; "gray5" ; "gray6" ; "gray7" ; "gray8" ; 
"gray9" ; "gray10" ; "gray11" ; "gray12" ; "gray13" ; 
"gray14" ; "gray15" ; "gray16" ; "gray17" ; "gray18" ; 
"gray19" ; "gray20" ; "gray21" ; "gray22" ; "gray23" ; 
"gray24" ; "gray25" ; "gray26" ; "gray27" ; "gray28" ; 
"gray29" ; "gray30" ; "gray31" ; "gray32" ; "gray33" ; 
"gray34" ; "gray35" ; "gray36" ; "gray37" ; "gray38" ; 
"gray39" ; "gray40" ; "gray41" ; "gray42" ; "gray43" ; 
"gray44" ; "gray45" ; "gray46" ; "gray47" ; "gray48" ; 
"gray49" ; "gray50" ; "gray51" ; "gray52" ; "gray53" ; 
"gray54" ; "gray55" ; "gray56" ; "gray57" ; "gray58" ; 
"gray59" ; "gray60" ; "gray61" ; "gray62" ; "gray63" ; 
"gray64" ; "gray65" ; "gray66" ; "gray67" ; "gray68" ; 
"gray69" ; "gray70" ; "gray71" ; "gray72" ; "gray73" ; 
"gray74" ; "gray75" ; "gray76" ; "gray77" ; "gray78" ; 
"gray79" ; "gray80" ; "gray81" ; "gray82" ; "gray83" ; 
"gray84" ; "gray85" ; "gray86" ; "gray87" ; "gray88" ; 
"gray89" ; "gray90" ; "gray91" ; "gray92" ; "gray93" ; 
"gray94" ; "gray95" ; "gray96" ; "gray97" ; "gray98" ; 
"gray99" ; "gray100" ;*) (*"green" ;*) "green1" ; "green2" ; 
"green3" ; "green4" ; "greenyellow" ; "grey"; (* "grey0" ; 
"grey1" ; "grey2" ; "grey3" ; "grey4" ; "grey5" ; 
"grey6" ; "grey7" ; "grey8" ; "grey9" ; "grey10" ; 
"grey11" ; "grey12" ; "grey13" ; "grey14" ; "grey15" ; 
"grey16" ; "grey17" ; "grey18" ; "grey19" ; "grey20" ; 
"grey21" ; "grey22" ; "grey23" ; "grey24" ; "grey25" ; 
"grey26" ; "grey27" ; "grey28" ; "grey29" ; "grey30" ; 
"grey31" ; "grey32" ; "grey33" ; "grey34" ; "grey35" ; 
"grey36" ; "grey37" ; "grey38" ; "grey39" ; "grey40" ; 
"grey41" ; "grey42" ; "grey43" ; "grey44" ; "grey45" ; 
"grey46" ; "grey47" ; "grey48" ; "grey49" ; "grey50" ; 
"grey51" ; "grey52" ; "grey53" ; "grey54" ; "grey55" ; 
"grey56" ; "grey57" ; "grey58" ; "grey59" ; "grey60" ; 
"grey61" ; "grey62" ; "grey63" ; "grey64" ; "grey65" ; 
"grey66" ; "grey67" ; "grey68" ; "grey69" ; "grey70" ; 
"grey71" ; "grey72" ; "grey73" ; "grey74" ; "grey75" ; 
"grey76" ; "grey77" ; "grey78" ; "grey79" ; "grey80" ; 
"grey81" ; "grey82" ; "grey83" ; "grey84" ; "grey85" ; 
"grey86" ; "grey87" ; "grey88" ; "grey89" ; "grey90" ; 
"grey91" ; "grey92" ; "grey93" ; "grey94" ; "grey95" ; 
"grey96" ; "grey97" ; "grey98" ; "grey99" ; "grey100" ; *)
"honeydew" ; "honeydew1" ; "honeydew2" ; "honeydew3" ; "honeydew4" ; 
"hotpink" ; "hotpink1" ; "hotpink2" ; "hotpink3" ; "hotpink4" ; 
"indianred" ; "indianred1" ; "indianred2" ; "indianred3" ; "indianred4" ; 
"indigo" ; "ivory" ; "ivory1" ; "ivory2" ; "ivory3" ; 
"ivory4" ; "khaki" ; "khaki1" ; "khaki2" ; "khaki3" ; 
"khaki4" ; "lavender" ; "lavenderblush" ; "lavenderblush1" ; "lavenderblush2" ; 
"lavenderblush3" ; "lavenderblush4" ; "lawngreen" ; "lemonchiffon" ; "lemonchiffon1" ; 
"lemonchiffon2" ; "lemonchiffon3" ; "lemonchiffon4" ; "lightblue" ; "lightblue1" ; 
"lightblue2" ; "lightblue3" ; "lightblue4" ; "lightcoral" ; "lightcyan" ; 
"lightcyan1" ; "lightcyan2" ; "lightcyan3" ; "lightcyan4" ; "lightgoldenrod" ; 
"lightgoldenrod1" ; "lightgoldenrod2" ; "lightgoldenrod3" ; "lightgoldenrod4" ; "lightgoldenrodyellow" ; 
"lightgray" ; "lightgrey" ; "lightpink" ; "lightpink1" ; "lightpink2" ; 
"lightpink3" ; "lightpink4" ; "lightsalmon" ; "lightsalmon1" ; "lightsalmon2" ; 
"lightsalmon3" ; "lightsalmon4" ; "lightseagreen" ; "lightskyblue" ; "lightskyblue1" ; 
"lightskyblue2" ; "lightskyblue3" ; "lightskyblue4" ; "lightslateblue" ; "lightslategray" ; 
"lightslategrey" ; "lightsteelblue" ; "lightsteelblue1" ; "lightsteelblue2" ; "lightsteelblue3" ; 
"lightsteelblue4" ; "lightyellow" ; "lightyellow1" ; "lightyellow2" ; "lightyellow3" ; 
"lightyellow4" ; "limegreen" ; "linen" ; (*"magenta" ;*) "magenta1" ; 
"magenta2" ; "magenta3" ; "magenta4" ; "maroon" ; "maroon1" ; 
"maroon2" ; "maroon3" ; "maroon4" ; "mediumaquamarine" ; "mediumblue" ; 
"mediumorchid" ; "mediumorchid1" ; "mediumorchid2" ; "mediumorchid3" ; "mediumorchid4" ; 
"mediumpurple" ; "mediumpurple1" ; "mediumpurple2" ; "mediumpurple3" ; "mediumpurple4" ; 
"mediumseagreen" ; "mediumslateblue" ; "mediumspringgreen" ; "mediumturquoise" ; "mediumvioletred" ; 
"midnightblue" ; "mintcream" ; "mistyrose" ; "mistyrose1" ; "mistyrose2" ; 
"mistyrose3" ; "mistyrose4" ; "moccasin" ; "navajowhite" ; "navajowhite1" ; 
"navajowhite2" ; "navajowhite3" ; "navajowhite4" ; "   navy   " ; "navyblue" ; 
"oldlace" ; "olivedrab" ; "olivedrab1" ; "olivedrab2" ; "olivedrab3" ; 
"olivedrab4" ; "orange" ; "orange1" ; "orange2" ; "orange3" ; 
"orange4" ; "orangered" ; "orangered1" ; "orangered2" ; "orangered3" ; 
"orangered4" ; "orchid" ; "orchid1" ; "orchid2" ; "orchid3" ; 
"orchid4" ; "palegoldenrod" ; "palegreen" ; "palegreen1" ; "palegreen2" ; 
"palegreen3" ; "palegreen4" ; "paleturquoise" ; "paleturquoise1" ; "paleturquoise2" ; 
"paleturquoise3" ; "paleturquoise4" ; "palevioletred" ; "palevioletred1" ; "palevioletred2" ; 
"palevioletred3" ; "palevioletred4" ; "papayawhip" ; "peachpuff" ; "peachpuff1" ; 
"peachpuff2" ; "peachpuff3" ; "peachpuff4" ; "   peru   " ; "   pink   " ; 
"pink1" ; "pink2" ; "pink3" ; "pink4" ; "   plum   " ; 
"plum1" ; "plum2" ; "plum3" ; "plum4" ; "powderblue" ; 
"purple" ; "purple1" ; "purple2" ; "purple3" ; "purple4" ; 
(*"red" ;*) "red1" ; "   red2   " ; "   red3   " ; "   red4   " ; 
"rosybrown" ; "rosybrown1" ; "rosybrown2" ; "rosybrown3" ; "rosybrown4" ; 
"royalblue" ; "royalblue1" ; "royalblue2" ; "royalblue3" ; "royalblue4" ; 
"saddlebrown" ; "salmon" ; "salmon1" ; "salmon2" ; "salmon3" ; 
"salmon4" ; "sandybrown" ; "seagreen" ; "seagreen1" ; "seagreen2" ; 
"seagreen3" ; "seagreen4" ; "seashell" ; "seashell1" ; "seashell2" ; 
"seashell3" ; "seashell4" ; "sienna" ; "sienna1" ; "sienna2" ; 
"sienna3" ; "sienna4" ; "skyblue" ; "skyblue1" ; "skyblue2" ; 
"skyblue3" ; "skyblue4" ; "slateblue" ; "slateblue1" ; "slateblue2" ; 
"slateblue3" ; "slateblue4" ; "slategray" ; "slategray1" ; "slategray2" ; 
"slategray3" ; "slategray4" ; "slategrey" ; "   snow   " ; "snow1" ; 
"snow2" ; "snow3" ; "snow4" ; "springgreen" ; "springgreen1" ; 
"springgreen2" ; "springgreen3" ; "springgreen4" ; "steelblue" ; "steelblue1" ; 
"steelblue2" ; "steelblue3" ; "steelblue4" ; "   tan   " ; "   tan1   " ; 
"   tan2   " ; "   tan3   " ; "   tan4   " ; "thistle" ; "thistle1" ; 
"thistle2" ; "thistle3" ; "thistle4" ; "tomato" ; "tomato1" ; 
"tomato2" ; "tomato3" ; "tomato4" ; "transparent" ; "turquoise" ; 
"turquoise1" ; "turquoise2" ; "turquoise3" ; "turquoise4" ; "violet" ; 
"violetred" ; "violetred1" ; "violetred2" ; "violetred3" ; "violetred4" ; 
"wheat" ; "wheat1" ; "wheat2" ; "wheat3" ; "wheat4" ; 
"white" ; "whitesmoke" ; (*"yellow" ;*) "yellow1" ; "yellow2" ; 
"yellow3" ; "yellow4" ; "yellowgreen"
]*)

let dot_colors = [
(* I ordered the first colors *)
"red" ; "green" ; "blue" ; "yellow" ; "cyan" ; "magenta" ;
(* The rest : random ! *)
"paleturquoise2"; "indianred1"; "goldenrod3"; "darkolivegreen4"; "slategray4"; "turquoise4"; "lightpink"; "salmon"; "pink3"; "chocolate4"; "lightslateblue"; "yellow3"; "   red4   "; "seashell3"; "cyan"; "darkgoldenrod3"; "gainsboro"; "yellowgreen"; "peachpuff1"; "oldlace"; "khaki"; "deepskyblue"; "maroon3"; "gold3"; "   tan   "; "mediumblue"; "lightyellow"; "ivory"; "lightcyan"; "lightsalmon4"; "maroon2"; "maroon4"; "   tan3   "; "green2"; "ivory2"; "navyblue"; "wheat1"; "navajowhite3"; "darkkhaki"; "whitesmoke"; "goldenrod"; "gold1"; "sandybrown"; "springgreen3"; "magenta2"; "lightskyblue1"; "lightcyan3"; "khaki2"; "khaki3"; "lavender"; "orchid1"; "wheat"; "lavenderblush1"; "firebrick2"; "navajowhite4"; "darkslategray3"; "palegreen2"; "lavenderblush3"; "skyblue3"; "deepskyblue3"; "darkorange"; "magenta1"; "darkorange3"; "violetred1"; "lawngreen"; "deeppink3"; "darkolivegreen1"; "darkorange1"; "darkorchid1"; "limegreen"; "lightslategray"; "deeppink"; "   red2   "; "goldenrod1"; "mediumorchid4"; "cornsilk1"; "lemonchiffon3"; "   gold   "; "orchid"; "yellow2"; "lightpink4"; "violetred2"; "mediumpurple"; "lightslategrey"; "lightsalmon1"; "violetred"; "coral2"; "slategray"; "plum2"; "turquoise3"; "lightyellow3"; "green4"; "mediumorchid1"; "lightcyan1"; "lightsalmon3"; "green3"; "lightseagreen"; "mediumpurple1"; "lightskyblue"; "lightyellow2"; "firebrick"; "honeydew2"; "slateblue3"; "navajowhite"; "seagreen1"; "springgreen4"; "   peru   "; "springgreen2"; "mediumvioletred"; "ivory4"; "olivedrab3"; "lightyellow1"; "hotpink"; "sienna4"; "lightcyan4"; "chartreuse4"; "lemonchiffon4"; "indianred3"; "hotpink4"; "sienna1"; "slategray3"; "darkseagreen2"; "tomato3"; "honeydew3"; "mistyrose2"; "rosybrown1"; "pink2"; "powderblue"; "cornflowerblue"; "   tan1   "; "indianred4"; "slateblue2"; "palevioletred3"; "ivory1"; "honeydew4"; "white"; "wheat3"; "steelblue4"; "purple2"; "deeppink4"; "royalblue4"; "lightgrey"; "forestgreen"; "palegreen"; "darkorange4"; "lightsteelblue2"; "tomato4"; "royalblue1"; "hotpink1"; "hotpink3"; "palegoldenrod"; "orange3"; "yellow1"; "orange2"; "slateblue"; "lightblue"; "lavenderblush2"; "chartreuse3"; "hotpink2"; "lightblue1"; "coral1"; "orange1"; "gold2"; "lightcoral"; "mediumseagreen"; "darkgreen"; "dodgerblue1"; "khaki1"; "khaki4"; "lightblue4"; "lightyellow4"; "firebrick3"; "crimson"; "olivedrab2"; "mistyrose3"; "lightsteelblue4"; "mediumpurple3"; "maroon"; "purple1"; "mediumorchid3"; "lightblue3"; "snow4"; "pink4"; "lightgray"; "lightsteelblue1"; "mistyrose"; "lightgoldenrodyellow"; "slategray1"; "peachpuff4"; "lightsalmon2"; "lightgoldenrod4"; "linen"; "darkgoldenrod1"; "goldenrod4"; "   navy   "; "lightcyan2"; "darkgoldenrod"; "mediumorchid2"; "lightsalmon"; "sienna"; "lightgoldenrod"; "plum1"; "orangered4"; "mistyrose1"; "mediumorchid"; "salmon1"; "chocolate3"; "palevioletred"; "purple3"; "turquoise"; "   snow   "; "paleturquoise"; "darkolivegreen"; "deepskyblue2"; "honeydew1"; "midnightblue"; "steelblue2"; "darkturquoise"; "dimgray"; "mediumpurple4"; "darkorchid"; "seashell2"; "cyan2"; "olivedrab1"; "royalblue2"; "violet"; "seagreen2"; "thistle3"; "cornsilk3"; "moccasin"; "magenta3"; "mediumslateblue"; "cadetblue3"; "mediumaquamarine"; "magenta4"; "mintcream"; "orangered3"; "mistyrose4"; "darkseagreen4"; "orangered"; "palegreen4"; "mediumspringgreen"; "saddlebrown"; "plum3"; "palegreen3"; "darkviolet"; "violetred3"; "orange"; "seagreen"; "springgreen1"; "deeppink2"; "navajowhite1"; "paleturquoise4"; "   tan4   "; "slategrey"; "lightsteelblue"; "azure3"; "salmon4"; "olivedrab4"; "darkorchid2"; "rosybrown"; "peachpuff2"; "springgreen"; "thistle2"; "   tan2   "; "aquamarine2"; "rosybrown4"; "palevioletred2"; "slateblue4"; "cyan4"; "red1"; "slateblue1"; "cornsilk2"; "ivory3"; "lightpink2"; "mediumpurple2"; "sienna2"; "chocolate1"; "lightsteelblue3"; "lightgoldenrod3"; "blueviolet"; "sienna3"; "orangered1"; "lightpink3"; "mediumturquoise"; "darkorange2"; "skyblue1"; "steelblue"; "seashell4"; "salmon2"; "lightpink1"; "skyblue4"; "darkslategray4"; "palevioletred4"; "orchid2"; "blue2"; "orchid3"; "peachpuff3"; "transparent"; "lavenderblush4"; "darkslategray1"; "lemonchiffon"; "papayawhip"; "maroon1"; "skyblue"; "chocolate"; "snow2"; "pink1"; "peachpuff"; "tomato1"; "blue1"; "dodgerblue2"; "orchid4"; "   plum   "; "orange4"; "purple"; "royalblue3"; "   pink   "; "floralwhite"; "palegreen1"; "dodgerblue4"; "chartreuse"; "bisque4"; "plum4"; "darkseagreen3"; "lightskyblue3"; "darkseagreen1"; "lightblue2"; "royalblue"; "   red3   "; "salmon3"; "palevioletred1"; "purple4"; "burlywood1"; "chocolate2"; "darkolivegreen3"; "goldenrod2"; "seashell1"; "indianred"; "brown2"; "lemonchiffon1"; "steelblue1"; "thistle1"; "yellow4"; "lightskyblue4"; "skyblue2"; "lemonchiffon2"; "thistle4"; "tomato2"; "violetred4"; "green1"; "greenyellow"; "paleturquoise1"; "chartreuse2"; "darkseagreen"; "turquoise2"; "cyan3"; "olivedrab"; "darkslategrey"; "firebrick4"; "lightgoldenrod1"; "seagreen3"; "seagreen4"; "tomato"; "firebrick1"; "steelblue3"; "orangered2"; "lavenderblush"; "cyan1"; "snow1"; "dodgerblue3"; "rosybrown2"; "indianred2"; "blanchedalmond"; "gold4"; "paleturquoise3"; "honeydew"; "bisque2"; "bisque3"; "snow3"; "brown"; "deeppink1"; "dimgrey"; "lightgoldenrod2"; "lightskyblue2"; "navajowhite2"; "seashell"; "black"; "cadetblue1"; "cadetblue2"; "darkslategray"; "wheat2"; "burlywood"; "brown1"; "deepskyblue4"; "darkslateblue"; "deepskyblue1"; "slategray2"; "darksalmon"; "burlywood3"; "dodgerblue"; "turquoise1"; "grey"; "ghostwhite"; "thistle"; "blue4"; "cornsilk"; "azure"; "darkgoldenrod2"; "darkslategray2"; "beige"; "burlywood2"; "coral3"; "indigo"; "darkorchid4"; "coral"; "burlywood4"; "brown3"; "cornsilk4"; "wheat4"; "darkgoldenrod4"; "cadetblue4"; "brown4"; "cadetblue"; "azure4"; "darkolivegreen2"; "rosybrown3"; "coral4"; "azure2"; "blue3"; "chartreuse1"; "bisque1"; "aquamarine1"; "azure1"; "bisque"; "aquamarine4"; "antiquewhite3"; "antiquewhite2"; "darkorchid3"; "antiquewhite4"; "aquamarine3"; "aquamarine"; "antiquewhite"; "antiquewhite1"; "aliceblue"
]

(*(* Shuffle dot colors: should be executed only once!! *)
let shuffle_dot_colors =
	let shuffle = 
		Array.sort (fun _ _ -> (Random.int 3) - 1)
	in
	let colors = Array.of_list dot_colors in
	shuffle colors;
	Array.iter (fun color ->
		print_string ("\"" ^ color ^ "\"; ");
	) colors;
	terminate_program();*)



(* Convert a graph to a dot file *)
let dot_of_graph program pi0 reachability_graph =	
	let states = reachability_graph.states in
	let transitions = reachability_graph.transitions_table in
	(* Create the array of dot colors *)
	let dot_colors = Array.of_list dot_colors in
	(* Coloring function for each location *)
	let color = fun location_index ->
		(* If more colors than our array: white *)
		try dot_colors.(location_index) with Invalid_argument _ -> "white"
	in
	(* Array location_index -> location *)
	let locations = DynArray.create () in
	
	let header =
		(* Header *)
		"/***************************************************"
		^ "\n * File automatically generated for file '" ^ program.program_name ^ "'"
		^ (if program.imitator_mode = Reachability_analysis then "\n * Reachability analysis" else (
			"\n * The following pi0 was considered:"
			^ "\n" ^ (ImitatorPrinter.string_of_pi0 program pi0)
		))
		^ "\n * " ^ (string_of_int (DynArray.length states)) ^ " states and "
			^ (string_of_int (Hashtbl.length transitions)) ^ " transitions"
		^ "\n * Program terminated " ^ (after_seconds ())
		^ "\n***************************************************/"
	in
	
	let states_description =	
		(* Give the state indexes in comments *)
		  "\n\n/*"
		^ "\n  DESCRIPTION OF THE STATES"
		^
		(**** BAD PROG ****)
		(let string_states = ref "" in
			DynArray.iteri (fun state_index state ->
			(* Construct the string *)
			string_states := !string_states
				(* Add the state *)
				^ "\n\n\n  STATE " ^ (string_of_int state_index) ^ ":"
				^ "\n  " ^ (ImitatorPrinter.string_of_state program state)
				(* Add the constraint with no clocks (option only) *)
				^ (if program.with_parametric_log then (
					(* Get the constraint *)
					let _, linear_constraint = state in
					(* Eliminate clocks *)
					let parametric_constraint = LinearConstraint.hide program.clocks linear_constraint in
					"\n\n  After clock elimination:"
					^ "\n  " ^ (LinearConstraint.string_of_linear_constraint program.variable_names parametric_constraint);
				) else "");
			) states;
		!string_states)
		^ "\n*/"
	in
	
	let dot_file =
		"\n\ndigraph G {"
		(* Convert the transitions *)
		^ (Hashtbl.fold (fun (orig_state_index, action_index) dest_state_index my_string ->
			my_string
			^ "\n  "
			^ "q_" ^ (string_of_int orig_state_index)
			^ " -> "
			^ "q_" ^ (string_of_int dest_state_index)
			^ " [label=\"" ^ (program.action_names action_index) ^ "\"];"

		) transitions "")

	(*	(* Add a nice color *)
		^ "\n\n  q_0 [color=red, style=filled];"
		^ "\n}"*)
		(* Add nice colors *)
		^ "\n" ^
		(**** BAD PROG ****)
		(let string_colors = ref "" in
			DynArray.iteri (fun state_index (location, _) ->
			(* Find the location index *)
			let location_index = try
				(**** BAD PROG: should be hashed ****)
				(* If the location index exists: return it *)
				DynArray.index_of (fun some_location -> some_location = location) locations
				(* Else add the location *)
				with Not_found -> (DynArray.add locations location; DynArray.length locations - 1)
			in
			(* Find the location color *)
			let location_color = color location_index in
			(* Create the command *)
			string_colors := !string_colors
				^ "\n  q_" ^ (string_of_int state_index)
				^ "[color=" ^ location_color
				^ ", style=filled];";
			) states;
		!string_colors)
		^ "\n}"

	in
	(* Dot file *)
	header ^ dot_file,
	(* Description of the states *)
	header ^ states_description ^ dot_file
