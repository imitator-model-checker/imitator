open Model_gen
open PZone

type level = None_l | NonStrict_l | Strict_l

type zone_class = {
  clk_lb : level;
  clk_ub : level;
  prm_lb : level;
  prm_ub : level;
  cpl    : level;
}

let level_str = function
  | None_l      -> "None     "
  | NonStrict_l -> "NonStrict"
  | Strict_l    -> "Strict   "

let level_str_json = function
  | None_l      -> "none"
  | NonStrict_l -> "nonstrict"
  | Strict_l    -> "strict"

let level_of_lb (box : Box.t) : level =
  let has_any = Array.exists (fun (iv : Interval.t) -> iv.lb <> None) box in
  if not has_any then None_l
  else
    let has_strict = Array.exists (fun (iv : Interval.t) ->
      match iv.lb with Some (Strictness.Strict, _) -> true | _ -> false) box in
    if has_strict then Strict_l else NonStrict_l

let level_of_ub (box : Box.t) : level =
  let has_any = Array.exists (fun (iv : Interval.t) -> iv.ub <> None) box in
  if not has_any then None_l
  else
    let has_strict = Array.exists (fun (iv : Interval.t) ->
      match iv.ub with Some (Strictness.Strict, _) -> true | _ -> false) box in
    if has_strict then Strict_l else NonStrict_l

let level_of_coupling (coupling : Coupling.t) : level =
  let has_any = Array.exists (fun c -> c <> ParamBound.NoBound) coupling in
  if not has_any then None_l
  else
    let has_strict = Array.exists (function
      | ParamBound.Upper (Strictness.Strict, _)
      | ParamBound.Lower (Strictness.Strict, _) -> true
      | _ -> false) coupling in
    if has_strict then Strict_l else NonStrict_l

let classify (pz : t) : zone_class = {
  clk_lb = level_of_lb pz.clocks;
  clk_ub = level_of_ub pz.clocks;
  prm_lb = level_of_lb pz.params;
  prm_ub = level_of_ub pz.params;
  cpl    = level_of_coupling pz.coupling;
}

type direction = Upper_only | Lower_only | Mixed_dir

let coupling_direction (pz : t) : direction option =
  let has_upper = Array.exists (function ParamBound.Upper _ -> true | _ -> false) pz.coupling in
  let has_lower = Array.exists (function ParamBound.Lower _ -> true | _ -> false) pz.coupling in
  match has_upper, has_lower with
  | false, false -> None
  | true,  false -> Some Upper_only
  | false, true  -> Some Lower_only
  | true,  true  -> Some Mixed_dir

(* --- Incremental state --- *)

type counts3 = {
  mutable none      : int;
  mutable nonstrict : int;
  mutable strict    : int;
}

let make_counts3 () = { none = 0; nonstrict = 0; strict = 0 }

let incr3 c = function
  | None_l      -> c.none      <- c.none      + 1
  | NonStrict_l -> c.nonstrict <- c.nonstrict + 1
  | Strict_l    -> c.strict    <- c.strict    + 1

let counts3_to_list c = [c.none; c.nonstrict; c.strict]

type state = {
  mutable n               : int;
  clk_lb                  : counts3;
  clk_ub                  : counts3;
  prm_lb                  : counts3;
  prm_ub                  : counts3;
  cpl                     : counts3;
  mutable n_with_coupling : int;
  mutable n_upper_only    : int;
  mutable n_lower_only    : int;
  mutable n_mixed_dir     : int;
  combo_tbl               : (zone_class, unit) Hashtbl.t;
}

let create () = {
  n = 0;
  clk_lb = make_counts3 (); clk_ub = make_counts3 ();
  prm_lb = make_counts3 (); prm_ub = make_counts3 ();
  cpl    = make_counts3 ();
  n_with_coupling = 0; n_upper_only = 0; n_lower_only = 0; n_mixed_dir = 0;
  combo_tbl = Hashtbl.create 256;
}

let update (s : state) (pz : t) =
  s.n <- s.n + 1;
  let cls = classify pz in
  incr3 s.clk_lb cls.clk_lb;
  incr3 s.clk_ub cls.clk_ub;
  incr3 s.prm_lb cls.prm_lb;
  incr3 s.prm_ub cls.prm_ub;
  incr3 s.cpl    cls.cpl;
  (match coupling_direction pz with
  | None            -> ()
  | Some Upper_only -> s.n_with_coupling <- s.n_with_coupling + 1;
                       s.n_upper_only    <- s.n_upper_only    + 1
  | Some Lower_only -> s.n_with_coupling <- s.n_with_coupling + 1;
                       s.n_lower_only    <- s.n_lower_only    + 1
  | Some Mixed_dir  -> s.n_with_coupling <- s.n_with_coupling + 1;
                       s.n_mixed_dir     <- s.n_mixed_dir     + 1);
  Hashtbl.replace s.combo_tbl cls ()

let report (s : state) ~(results_file : string option) =
  let n = s.n in
  if n = 0 then (print_endline "No guards generated."; exit 0);

  let pct k = 100.0 *. float_of_int k /. float_of_int n in
  let levels = [None_l; NonStrict_l; Strict_l] in
  let distinct = Hashtbl.length s.combo_tbl in
  (* 243 - 1: the all-None zone is unreachable because bounded_geo starts at 1,
     so at least one atom is always kept. *)
  let max_combinations = 242 in

  let sep = String.make 46 '-' in
  Printf.printf "\n=== Guard Coverage Report (%d zones) ===\n\n" n;

  let print_dim label counts =
    Printf.printf "%s:\n" label;
    List.iter2 (fun l k ->
      Printf.printf "  %s  %6d  (%5.1f%%)\n" (level_str l) k (pct k)
    ) levels counts;
    print_newline ()
  in
  print_dim "Clock lb level" (counts3_to_list s.clk_lb);
  print_dim "Clock ub level" (counts3_to_list s.clk_ub);
  print_dim "Param lb level" (counts3_to_list s.prm_lb);
  print_dim "Param ub level" (counts3_to_list s.prm_ub);
  print_dim "Coupling level" (counts3_to_list s.cpl);

  Printf.printf "Coupling direction (of %d with coupling):\n" s.n_with_coupling;
  let dir_pct k = if s.n_with_coupling = 0 then 0.0
    else 100.0 *. float_of_int k /. float_of_int s.n_with_coupling in
  Printf.printf "  Upper-only  %6d  (%5.1f%%)\n" s.n_upper_only (dir_pct s.n_upper_only);
  Printf.printf "  Lower-only  %6d  (%5.1f%%)\n" s.n_lower_only (dir_pct s.n_lower_only);
  Printf.printf "  Mixed       %6d  (%5.1f%%)\n" s.n_mixed_dir  (dir_pct s.n_mixed_dir);
  print_newline ();

  Printf.printf "%s\n" sep;
  Printf.printf "Distinct combinations: %d / %d achievable (%5.1f%%)\n"
    distinct max_combinations
    (100.0 *. float_of_int distinct /. float_of_int max_combinations);

  match results_file with
  | None -> ()
  | Some path ->
    let oc = open_out path in
    let fpf fmt = Printf.fprintf oc fmt in
    fpf "{\n";
    fpf "  \"total\": %d,\n" n;
    let print_dim_json key counts =
      fpf "  %S: {\n" key;
      List.iter2 (fun l k ->
        fpf "    %S: {\"count\": %d, \"pct\": %.1f},\n" (level_str_json l) k (pct k)
      ) levels counts;
      fpf "  },\n"
    in
    print_dim_json "clock_lb_level" (counts3_to_list s.clk_lb);
    print_dim_json "clock_ub_level" (counts3_to_list s.clk_ub);
    print_dim_json "param_lb_level" (counts3_to_list s.prm_lb);
    print_dim_json "param_ub_level" (counts3_to_list s.prm_ub);
    print_dim_json "coupling_level" (counts3_to_list s.cpl);
    fpf "  \"coupling_direction\": {\n";
    fpf "    \"with_coupling\": %d,\n" s.n_with_coupling;
    fpf "    \"upper_only\": {\"count\": %d, \"pct\": %.1f},\n" s.n_upper_only (dir_pct s.n_upper_only);
    fpf "    \"lower_only\": {\"count\": %d, \"pct\": %.1f},\n" s.n_lower_only (dir_pct s.n_lower_only);
    fpf "    \"mixed\": {\"count\": %d, \"pct\": %.1f}\n" s.n_mixed_dir (dir_pct s.n_mixed_dir);
    fpf "  },\n";
    fpf "  \"distinct_combinations\": %d,\n" distinct;
    fpf "  \"max_achievable_combinations\": %d\n" max_combinations;
    (* Note: 243 - 1; all-None is unreachable since bounded_geo starts at 1 *)
    fpf "}\n";
    close_out oc
