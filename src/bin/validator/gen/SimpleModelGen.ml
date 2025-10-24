open Crowbar 
open SimpleModel


let const_int_from_sampler sampler = 
  let sample = Sampler.next_int sampler 10 in 
  SConstant sample

let variable_from_sampler sampler ~nb_clocks ~nb_parameters = 
  let coinflip = Sampler.next_bool sampler ~prob:0.5 in
  if nb_parameters = 0 || coinflip then 
    let sample = Sampler.next_int sampler nb_clocks in 
    SClock sample
  else
    let sample = Sampler.next_int sampler nb_parameters in 
    SParam sample
  
  

let comparison_from_sampler sampler ~nb_clocks ~nb_parameters ~opers = 
  let clock = variable_from_sampler sampler ~nb_clocks ~nb_parameters in 
  let constant = const_int_from_sampler sampler in 
  let oper = Sampler.sample_uniform sampler ~from:opers in 
  SComp (clock, oper, constant)

let bool_expr_from_sampler sampler ~nb_clocks ~nb_parameters ~opers = 
  let comparison = comparison_from_sampler sampler ~nb_clocks ~nb_parameters ~opers in
  [comparison]
  
let resets_from_sampler sampler nb_clocks =
  (* each clock has an independent chance to be added to the list of resets *)
  let should_add_reset () = Sampler.next_bool sampler ~prob:0.1 in
  let rec compute_resets acc = function
  | 0 -> acc
  | n -> if should_add_reset () then compute_resets ((n - 1)  :: acc) (n - 1) else acc in 
  compute_resets [] nb_clocks

let transition_from_sampler sampler ~nb_clocks ~nb_parameters =
  let guard = bool_expr_from_sampler sampler ~nb_clocks ~nb_parameters ~opers:[EQ; GEQ; G; L; LEQ] in 
  let controllable = Sampler.next_bool sampler ~prob:0.5 in 
  let resets = resets_from_sampler sampler nb_clocks in
  {controllable; guard; resets}

(* 
  Probability to be accepting = (i/(n-1))^2, scaled down such that the
  expected number of accepting locations per automaton = 1  
*)
let prob_accepting n i = 
  if n <= 1 then 1.0 else 

  let num = float_of_int (6 * i * i) in 
  let denom = float_of_int ((n - 1) * n * (2 * n - 1)) in

  num /. denom  


let accepting_locations_from_sampler ~nb_auto ~nb_loc_of_automaton ~sampler ~guarantee_accepting  =
  Array.init nb_auto (fun i ->
    let accepting_loc_exists = ref false in 
    Array.init nb_loc_of_automaton.(i) (fun j ->
      let prob = prob_accepting (nb_loc_of_automaton.(i)) j in
      let accepting = Sampler.next_bool sampler ~prob in
      if accepting then accepting_loc_exists := true;
      let forced_accepting =  j = nb_loc_of_automaton.(i) - 1 && not !accepting_loc_exists && guarantee_accepting in
      accepting || forced_accepting 
    ))

let invariants_from_sampler ~nb_auto ~nb_loc_of_automaton ~sampler ~nb_clocks ~nb_parameters = 
  Array.init nb_auto (fun i -> 
    Array.init nb_loc_of_automaton.(i) (fun _ ->
      bool_expr_from_sampler sampler ~nb_clocks ~nb_parameters ~opers:[L; LEQ]
    ))

let transition_of_edge sampler nb_clocks nb_parameters has_edge =
  if has_edge then Some (transition_from_sampler sampler ~nb_clocks ~nb_parameters)
  else None

let generate_transition_matrix ~sampler ~density ~nb_clocks ~nb_parameters ~nb_locations =
  Adjacency.generate ~sampler ~nodes:nb_locations ~density
  |> Array.map (Array.map (transition_of_edge sampler nb_clocks nb_parameters))
let gen : t gen =                 
  map [range ~min:1 1; range ~min:1 2; range ~min:1 1; bytes_fixed 128] 
    (fun nb_auto nb_clocks nb_parameters random_blob ->
      let sampler = Sampler.create ~seed:random_blob in 
      let nb_loc_of_automaton = Array.init nb_auto (fun _ -> Sampler.next_int sampler ~min:10 5) in 
      let matrices =
      List.init nb_auto (fun a_id ->
        generate_transition_matrix ~nb_locations:(nb_loc_of_automaton.(a_id)) ~sampler ~density:0.1 ~nb_clocks ~nb_parameters)
      in
      let accepting = accepting_locations_from_sampler ~nb_auto ~nb_loc_of_automaton ~sampler ~guarantee_accepting:true in 
      let invariants = invariants_from_sampler ~nb_auto ~nb_loc_of_automaton ~nb_clocks ~nb_parameters ~sampler in
      { automata = matrices; accepting; nb_clocks; nb_parameters; invariants})
