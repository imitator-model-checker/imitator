
type t = int * bool array

(* Get a zero binary word of length l *)
let zero l =
    l, Array.init l (fun i -> false)

let length (l, b) =
    l

(* Convert string value to binary word type *)
let binaryword_of_string s =
    (* Extract binary part of 0bxxxx *)
    let str_binary_part = String.sub s 2 ((String.length s) - 2) in
    (* Then convert 0 chars to false and other chars to true *)
    let l = String.length str_binary_part in
    l, Array.init (String.length str_binary_part) (fun i -> if String.get str_binary_part i = '0' then false else true)

(* Convert binary word to string *)
let string_of_binaryword (_, b) =
    let str_l = Array.map (fun bit -> if bit then "1" else "0") b in
    "0b" ^ Array.fold_left (fun a b -> a ^ b) "" str_l

let to_string = string_of_binaryword

let shift_left (l, b) i =
    l, Array.append (Array.sub b i ((Array.length b) - i)) (Array.make i false)

let shift_right (l, b) i =
    l, Array.append (Array.make i false) (Array.sub b 0 ((Array.length b) - i))

let fill_left (l, b) i =
    l + i, Array.append b (Array.make i false)

let fill_right (l, b) i =
    l + i, Array.append (Array.make i false) b

let log_not (l, b) = l, Array.map not b

let bitwise op (l1, b1) (l2, b2) =
    l1, Array.mapi (fun i a -> op (Array.get b1 i) (Array.get b2 i)) b1

let log_and = bitwise (&&)
let log_or = bitwise (||)

let bool_lxor a b =
    match a, b with
    | false, false -> false
    | true, false -> true
    | false, true -> true
    | true, true -> false

let log_xor = bitwise (bool_lxor)



let hash (_, b) =
    let remap = Array.mapi (fun i x -> i + 1, if x then 1 else 0) b in
    Array.fold_left (fun a (i, s) -> a + i * s) 0 remap

let compare op a b =
    op (hash a) (hash b)

let l = compare (<)
let leq = compare (<=)
let g = compare (>)
let geq = compare (>=)
let equal a b = a = b

let neq a b =
    not (equal a b)

let (=) = equal
let (<>) = neq
let (<) = l
let (<=) = leq
let (>) = g
let (>=) = geq

let to_array b = let _, a = b in a