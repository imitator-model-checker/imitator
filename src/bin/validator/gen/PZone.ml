module Const = struct type t = int end
module Clock = struct type t = int end
module Param = struct type t = int end


module Strictness = struct 
  type t = Strict | NonStrict
  let compare s1 s2 =
    match s1, s2 with
    | Strict, Strict
    | NonStrict, NonStrict -> 0
    | Strict, NonStrict -> -1
    | NonStrict, Strict -> 1
  let min a b = if compare a b <= 0 then a else b
  let to_string_ub = function Strict -> ")" | NonStrict -> "]"

  let to_string_lb = function Strict -> "(" | NonStrict -> "["
end



type zval = Const.t array

module Valuation = struct 
  type t = {
    params : zval;
    clocks : zval;
  }
end

module ConstBound = struct 
  type t = (Strictness.t * Const.t) option

  let make strict const : t = Some (strict, const)



  let compare_raw (s1,a) (s2,b) =
    let c = Int.compare a b in
    if c <> 0 then c else Strictness.compare s1 s2

  (* Total *)
  let compare (x:t) (y:t) =
    match x,y with
    | None,None -> 0
    | None,Some _ -> -1
    | Some _,None -> 1
    | Some r1, Some r2 -> compare_raw r1 r2

  (* policy: prefer Some for both min/max - not technically total *)
  let min a b =
    match a,b with
    | Some _, None -> a
    | None, Some _ -> b
    | _ -> if compare a b <= 0 then a else b

  let max a b =
    match a,b with
    | Some _, None -> a
    | None, Some _ -> b
    | _ -> if compare a b >= 0 then a else b
end

module Interval = struct
  type t = { lb : ConstBound.t; ub : ConstBound.t }
  let top = { lb = None; ub = None }
  let intersect a b = { lb = ConstBound.max a.lb b.lb; ub = ConstBound.min a.ub b.ub }
  let make_point n = { lb = ConstBound.make Strictness.NonStrict n; ub = ConstBound.make Strictness.NonStrict n}

  let to_string {ub;lb} = 
    let ub_str = match ub with 
      | Some (s,b) -> Printf.sprintf "%d%s" b @@ Strictness.to_string_ub s
      | None -> "∞)"
    in
    let lb_str = match lb with 
      | Some (s,b) -> Printf.sprintf "%s%d" (Strictness.to_string_lb s) b
      | None -> "(-∞"
    in
    Printf.sprintf "%s,%s" lb_str ub_str

end

module ParamBound = struct 
  type t = 
  | Upper of (Strictness.t * Param.t)
  | Lower of (Strictness.t * Param.t)
  | NoBound

  let instantiate (pi : zval) = function
  | Upper (s,p) -> { Interval.top with ub = ConstBound.make s pi.(p) }
  | Lower (s,p) -> { Interval.top with lb = ConstBound.make s pi.(p) }
  | NoBound -> Interval.top

  let to_string = function
  | Upper(s,p) -> Printf.sprintf "%s p[%d]" (if s = Strict then "<" else "<=") p
  | Lower(s,p) -> Printf.sprintf "%s p[%d]" (if s = Strict then ">" else ">=") p
  | NoBound -> ""
end

module Box = struct 
  type t = Interval.t array
  let top nb_vars = Array.init nb_vars (fun _ -> Interval.top)
  let intersect = Array.map2 Interval.intersect
  let to_string var_str b = 
    b
    |> Array.map Interval.to_string
    |> Array.mapi @@ Printf.sprintf "%s[%d] ∈ %s" var_str
    |> Array.to_list
    |> String.concat "\n"
end

module Coupling = struct 
  type t = ParamBound.t array
  let top nb_clocks = Array.init nb_clocks (fun _ -> ParamBound.NoBound)
  let to_string c = 
    c
    |> Array.to_list
    |> List.map ParamBound.to_string
    |> List.mapi (fun i pb_s -> (i, pb_s))
    |> List.filter (fun (_, pb_s) -> pb_s <> "")
    |> List.map (fun (i, s) -> Printf.sprintf "x[%d] %s" i s)
    |> String.concat "\n"
end


(* Semi-PDBM style zone representation *)
type t = {
  clocks : Box.t;
  params : Box.t;
  coupling : Coupling.t
}

let top ~nb_clocks ~nb_parameters : t = {
  clocks = Box.top nb_clocks;
  params = Box.top nb_parameters;
  coupling = Coupling.top nb_clocks;
}

let zero_clock ~nb_clocks ~nb_parameters = {
  clocks = Array.make nb_clocks (Interval.make_point 0);
  params = Box.top nb_parameters;
  coupling = Coupling.top nb_clocks;
}

let to_string {clocks;params;coupling} = 
  Printf.sprintf "===Clocks===\n%s\n===Params===\n%s\n===Coupling===\n%s"
  (Box.to_string "x" clocks)
  (Box.to_string "p" params)
  (Coupling.to_string coupling)


let instantiate (pi : zval) (pz : t) : Box.t =
  let nb_params = Array.length pz.params in
  if Array.length pi <> nb_params then
    invalid_arg "instantiate: parameter valuation has wrong size";

  Array.map2
  (fun param_bound clock_bound -> 
    param_bound
    |> ParamBound.instantiate pi
    |> Interval.intersect clock_bound
  )
  pz.coupling pz.clocks

let propagate (pz : t) : t =
  let nb_parameters = Array.length pz.params in
  let nb_clocks = Array.length pz.clocks in 
  let implied_params : Box.t = Box.top nb_parameters in
  let implied_clocks : Box.t = Box.top nb_clocks in 

  let add_param_lb (p : Param.t) (s : Strictness.t) (k : Const.t) =
    let cur = implied_params.(p) in
    implied_params.(p) <- Interval.intersect cur { Interval.top with lb = ConstBound.make s k }
  in
  let add_param_ub (p : Param.t) (s : Strictness.t) (k : Const.t) =
    let cur = implied_params.(p) in
    implied_params.(p) <- Interval.intersect cur { Interval.top with ub = ConstBound.make s k }
  in

  let add_clock_lb (c : Clock.t) (s : Strictness.t) (k : Const.t) = 
    let cur = implied_clocks.(c) in
    implied_clocks.(c) <- Interval.intersect cur { Interval.top with lb = ConstBound.make s k }
  in

  let add_clock_ub (c : Clock.t) (s : Strictness.t) (k : Const.t) = 
    let cur = implied_clocks.(c) in
    implied_clocks.(c) <- Interval.intersect cur { Interval.top with ub = ConstBound.make s k }
  in

  let clock_lb_with_implicit (lb : ConstBound.t) : Strictness.t * Const.t =
    match ConstBound.max lb (ConstBound.make NonStrict 0) with
    | None -> (NonStrict, 0)
    | Some r -> r
  in

  (* Strengthen params as much as possible (several clocks may strenghten a parameter) *) 
  Array.iter2
    (fun (kappa : ParamBound.t) (clock_bound : Interval.t) ->
      match kappa with
      | ParamBound.NoBound ->
          ()
      | ParamBound.Upper (s_up, p) ->
          let (s_lb, k_lb) = clock_lb_with_implicit clock_bound.lb in
          let s_p = Strictness.min s_lb s_up in
          add_param_lb p s_p k_lb

      | ParamBound.Lower (s_low, p) ->
          begin match clock_bound.ub with
          | None -> ()
          | Some (s_ub, k_ub) ->
              let s_p = Strictness.min s_ub s_low in
              add_param_ub p s_p k_ub
          end
    )
    pz.coupling pz.clocks;

  (* Strenghten clocks with propagated params *)
  Array.iteri
    (fun clock_id (kappa : ParamBound.t) ->
      match kappa with
      | ParamBound.NoBound ->
          ()
      | ParamBound.Lower (s_low, p) ->
          let (s_lb, k_lb) = clock_lb_with_implicit pz.params.(p).lb in
          let s_p = Strictness.min s_lb s_low in
          add_clock_lb clock_id s_p k_lb


      | ParamBound.Upper (s_up, p) ->
          begin match pz.params.(p).ub with
          | None -> ()
          | Some (s_ub, k_ub) ->
              let s_p = Strictness.min s_ub s_up in
              add_clock_ub clock_id s_p k_ub
          end
    ) pz.coupling;

  {pz with clocks = Box.intersect pz.clocks implied_clocks; params = Box.intersect pz.params implied_params}

let sample_valuation_from_zone (z : Box.t) ~(max_constant:int) : zval  =
  Array.map
    (fun (vb : Interval.t) ->
      let lb =
        match vb.lb with
        | None -> 0
        | Some (Strict, c) -> c + 1
        | Some (NonStrict, c) -> c
      in
      let ub =
        match vb.ub with
        | None -> max_constant
        | Some (Strict, c) -> c - 1
        | Some (NonStrict, c) -> c
      in
       if lb > ub then
         invalid_arg "sample_valuation_from_zone: empty bound";
       (lb + ub) / 2 (* midpoint *)
    )
    z
  
let witness (pz : t) ~max_constant : Valuation.t =
  let pz' = propagate pz in 
  let pi : zval = sample_valuation_from_zone pz'.params ~max_constant in
  let z : Box.t = instantiate pi { pz with params = pz'.params } in
  let nu : zval = sample_valuation_from_zone z ~max_constant in
  { params = pi; clocks = nu }


let membership (v : Valuation.t) (pz : t) : bool =
  let within_bound_lb (x : int) (b : ConstBound.t) : bool =
    match b with
    | None -> true
    | Some (Strictness.Strict, c) -> x > c
    | Some (Strictness.NonStrict, c) -> x >= c
  in
  let within_bound_ub (x : int) (b : ConstBound.t) : bool =
    match b with
    | None -> true
    | Some (Strictness.Strict, c) -> x < c
    | Some (Strictness.NonStrict, c) -> x <= c
  in
  let within_interval (x : int) (i : Interval.t) : bool =
    within_bound_lb x i.lb && within_bound_ub x i.ub
  in
  let within_box (vals : zval) (b : Box.t) : bool =
    let n = Array.length b in
    Array.length vals = n
    && (let ok = ref true in
        for i = 0 to n - 1 do
          ok := !ok && within_interval vals.(i) b.(i)
        done;
        !ok)
  in
  let nonneg_array (a : int array) =
    let ok = ref true in
    for i = 0 to Array.length a - 1 do
      ok := !ok && a.(i) >= 0
    done;
    !ok
  in

  (* basic size checks *)
  if Array.length v.params <> Array.length pz.params then
    false
  else if Array.length v.clocks <> Array.length pz.clocks then
    false
  else if Array.length v.clocks <> Array.length pz.coupling then
    false
  else
    (* assuming TA valuations are non-negative *)
    nonneg_array v.params
    && nonneg_array v.clocks
    (* parameter constraints *)
    && within_box v.params pz.params
    (* clock constraints + coupling instantiated by the parameter valuation *)
    && within_box v.clocks (instantiate v.params pz)