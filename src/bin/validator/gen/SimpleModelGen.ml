open QCheck2
open SimpleModel
open Validator_config

let gen_of_dist : Spec.dist -> int Gen.t = function 
  | Exact n -> Gen.pure n
  | Range (min, max) -> Gen.int_range ~origin:min min max

let bool_of_ratio ratio = 
  Gen.(
    let+ roll = float_bound_inclusive 1. in 
    roll <= ratio
  )
  
module Transition = struct 
  open Gen
  let resets nb_clocks prob =
    (* each clock has an independent chance to be added to the list of resets *)
    let rec compute_resets acc = function
    | 0 -> return acc
    | n -> 
      let* should_add_reset = bool_of_ratio prob in
      if should_add_reset then 
        compute_resets ((n - 1)  :: acc) (n - 1) 
      else return acc 
    in 
    compute_resets [] nb_clocks

  let transition ~nb_clocks ~nb_parameters ~(spec : Spec.t) =

    let* guard =  
      let* is_guard = bool_of_ratio spec.guard_probability in 
      if not is_guard then 
        return []
      else
        FormulaGen.formula ~nb_clocks ~nb_parameters ~opers:spec.guard_types in 
    let* controllable = bool_of_ratio spec.controllability_ratio in 
    let+ resets = resets nb_clocks spec.reset_probability in
    {controllable; guard; resets}
end




module Automaton = struct
  open Gen
  (* 
    Probability to be accepting = (i/(n-1))^2, scaled down such that the
    expected number of accepting locations per automaton = 1  
  *)
  let prob_accepting n i = 
    if n <= 1 then 1.0 else 

    let num = float_of_int (6 * i * i) in 
    let denom = float_of_int ((n - 1) * n * (2 * n - 1)) in

    num /. denom  


  let accepting_locations ~nb_auto ~nb_loc_of_automaton ~guarantee_accepting =
    flatten_a @@
    Array.init nb_auto (fun i ->
      let accepting_loc_exists = ref false in 
      flatten_a @@
      Array.init nb_loc_of_automaton.(i) (fun j ->
        let prob = prob_accepting (nb_loc_of_automaton.(i)) j in
        let+ accepting = bool_of_ratio prob in 
        if accepting then accepting_loc_exists := true;
        let forced_accepting =  j = nb_loc_of_automaton.(i) - 1 && not !accepting_loc_exists && guarantee_accepting in
        accepting || forced_accepting 
      ))

  let invariants ~nb_auto ~nb_loc_of_automaton ~nb_clocks ~nb_parameters ~(spec : Spec.t) = 
    flatten_a @@
    Array.init nb_auto (fun i -> 
      flatten_a @@
      Array.init nb_loc_of_automaton.(i) (fun j ->
        let* has_invariant = bool_of_ratio spec.invariant_probability in 
        if not has_invariant then 
          pure []
        else
          let opers = if j = 0 then 
            List.filter (fun op -> let open Spec in op <> S_GT && op <> S_GE && op <> S_EQ) spec.invariant_types
          else 
            spec.invariant_types 
          in
          if opers = [] then 
            pure []
          else
            FormulaGen.formula ~nb_clocks ~nb_parameters ~opers
      ))

  let transitions_of_edge nb_clocks nb_parameters spec nb_edges =
    flatten_l @@ List.init nb_edges (fun _ -> Transition.transition ~nb_clocks ~nb_parameters ~spec)

  let transition_matrix_of_adjacency_matrix nb_clocks nb_parameters spec adj = 
    adj
    |> Array.map (fun row -> 
      row 
      |> Array.map (fun edges -> transitions_of_edge nb_clocks nb_parameters spec edges)
      |> flatten_a
    )
    |> flatten_a

  let transition_matrix ~spec ~nb_clocks ~nb_parameters ~nb_locations =
    Adjacency.generate ~nb_locations ~spec
    >>= transition_matrix_of_adjacency_matrix nb_clocks nb_parameters spec

end

module Generator = struct 
  open Gen
  let gen (spec : Spec.t)  =
    let* nb_auto = (gen_of_dist spec.nb_automata) in
    let* nb_loc_per_automaton = array_repeat nb_auto (gen_of_dist spec.nb_locations) in

    let* nb_clocks = (gen_of_dist spec.nb_clocks)
    and* nb_parameters = (gen_of_dist spec.nb_parameters) in 

    let+ automata =
      flatten_l @@
      List.init nb_auto (fun i ->
        Automaton.transition_matrix
          ~nb_locations:nb_loc_per_automaton.(i) ~spec
          ~nb_clocks ~nb_parameters)
    and+ accepting =
      Automaton.accepting_locations
        ~nb_auto ~nb_loc_of_automaton:nb_loc_per_automaton
        ~guarantee_accepting:true
    and+ invariants =
      Automaton.invariants
        ~nb_auto ~nb_loc_of_automaton:nb_loc_per_automaton
        ~nb_clocks ~nb_parameters ~spec
    in
    { automata; accepting; nb_clocks; nb_parameters; invariants }
end

let gen = Generator.gen