open Crowbar 
open SimpleModel

module Expr = struct 
  let constant sampler = 
  let sample = Sampler.next_int sampler 10 in 
  SConstant sample

  let variable sampler ~nb_clocks ~nb_parameters = 
    let coinflip = Sampler.next_bool sampler ~prob:0.5 in
    if nb_parameters = 0 || coinflip then 
      let sample = Sampler.next_int sampler nb_clocks in 
      SClock sample
    else
      let sample = Sampler.next_int sampler nb_parameters in 
      SParam sample

  let comparison sampler ~nb_clocks ~nb_parameters ~opers = 
    let clock = variable sampler ~nb_clocks ~nb_parameters in 
    let constant = constant sampler in 
    let oper = Sampler.sample_uniform sampler ~from:opers in 
    SComp (clock, oper, constant)

  let formula sampler ~nb_clocks ~nb_parameters ~opers = 
    let comparison = comparison sampler ~nb_clocks ~nb_parameters ~opers in
    [comparison]
end
  
module Transition = struct 
  let resets sampler nb_clocks =
    (* each clock has an independent chance to be added to the list of resets *)
    let should_add_reset () = Sampler.next_bool sampler ~prob:0.1 in
    let rec compute_resets acc = function
    | 0 -> acc
    | n -> if should_add_reset () then compute_resets ((n - 1)  :: acc) (n - 1) else acc in 
    compute_resets [] nb_clocks

  let transition sampler ~nb_clocks ~nb_parameters =
    let guard = Expr.formula sampler ~nb_clocks ~nb_parameters ~opers:[EQ; GEQ; G; L; LEQ] in 
    let controllable = Sampler.next_bool sampler ~prob:0.5 in 
    let resets = resets sampler nb_clocks in
    {controllable; guard; resets}
end


module Automaton = struct
  (* 
    Probability to be accepting = (i/(n-1))^2, scaled down such that the
    expected number of accepting locations per automaton = 1  
  *)
  let prob_accepting n i = 
    if n <= 1 then 1.0 else 

    let num = float_of_int (6 * i * i) in 
    let denom = float_of_int ((n - 1) * n * (2 * n - 1)) in

    num /. denom  


  let accepting_locations ~nb_auto ~nb_loc_of_automaton ~sampler ~guarantee_accepting  =
    Array.init nb_auto (fun i ->
      let accepting_loc_exists = ref false in 
      Array.init nb_loc_of_automaton.(i) (fun j ->
        let prob = prob_accepting (nb_loc_of_automaton.(i)) j in
        let accepting = Sampler.next_bool sampler ~prob in
        if accepting then accepting_loc_exists := true;
        let forced_accepting =  j = nb_loc_of_automaton.(i) - 1 && not !accepting_loc_exists && guarantee_accepting in
        accepting || forced_accepting 
      ))

  let invariants ~nb_auto ~nb_loc_of_automaton ~sampler ~nb_clocks ~nb_parameters = 
    Array.init nb_auto (fun i -> 
      Array.init nb_loc_of_automaton.(i) (fun _ ->
        Expr.formula sampler ~nb_clocks ~nb_parameters ~opers:[L; LEQ]
      ))

  let transition_of_edge sampler nb_clocks nb_parameters has_edge =
    if has_edge then Some (Transition.transition sampler ~nb_clocks ~nb_parameters)
    else None

  let transition_matrix ~sampler ~density ~nb_clocks ~nb_parameters ~nb_locations =
    Adjacency.generate ~sampler ~nodes:nb_locations ~density
    |> Array.map (Array.map (transition_of_edge sampler nb_clocks nb_parameters))
end

module Generator = struct 
  let gen : t gen =
    map [range ~min:1 1; range ~min:1 2; range ~min:1 1; bytes_fixed 128]
      (fun nb_auto nb_clocks nb_parameters random_blob ->
        let sampler = Sampler.create ~seed:random_blob in
        let nb_loc =
          Array.init nb_auto (fun _ -> Sampler.next_int sampler ~min:10 5)
        in
        let automata =
          List.init nb_auto (fun i ->
            Automaton.transition_matrix
              ~nb_locations:nb_loc.(i) ~sampler ~density:0.1
              ~nb_clocks ~nb_parameters)
        in
        let accepting =
          Automaton.accepting_locations
            ~nb_auto ~nb_loc_of_automaton:nb_loc ~sampler
            ~guarantee_accepting:true
        in
        let invariants =
          Automaton.invariants
            ~nb_auto ~nb_loc_of_automaton:nb_loc
            ~nb_clocks ~nb_parameters ~sampler
        in
        { automata; accepting; nb_clocks; nb_parameters; invariants })
end

let gen = Generator.gen