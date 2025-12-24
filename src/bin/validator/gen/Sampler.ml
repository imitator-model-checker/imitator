open Validator_spec

type t = { seed : string; mutable i : int }

let create ~seed = {seed; i = 0}

let next_byte br =
  let c = String.get br.seed (br.i mod String.length br.seed) in
  br.i <- br.i + 1;
  int_of_char c

let next_bool br ~prob =
  let threshold = int_of_float (prob *. 255.) in 
  next_byte br < threshold

let next_int br ?(min = 0) range =
  assert (range <= 255);
  (next_byte br mod range) + min

let sample_uniform br ~from =
  let idx = next_int br (List.length from) in 
  List.nth from idx

let sample_dist t : Spec.dist -> int = function
  | Exact n -> n
  | Range (min, n) -> 
    print_endline (Printf.sprintf "Sampling from range (%d, %d)" min n);
    let n = next_int t ~min n in 
    print_endline (Printf.sprintf "Sampled value: %d" n);
    n
