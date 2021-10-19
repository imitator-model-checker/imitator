
type t = int * bool array

(* Get a zero binary word of length l *)
val zero : int -> t
val length : t -> int

val binaryword_of_string : string -> t
val string_of_binaryword : t -> string
val to_string : t -> string

val shift_left : t -> int -> t
val shift_right : t -> int -> t
val fill_left : t -> int -> t
val fill_right : t -> int -> t
val log_not : t -> t
val log_and : t -> t -> t
val log_or : t -> t -> t
val log_xor : t -> t -> t

val l : t -> t -> bool
val leq : t -> t -> bool
val g : t -> t -> bool
val geq : t -> t -> bool
val equal : t -> t -> bool
val neq : t -> t -> bool

val (=) : t -> t -> bool
val (<>) : t -> t -> bool
val (<) : t -> t -> bool
val (<=) : t -> t -> bool
val (>) : t -> t -> bool
val (>=) : t -> t -> bool


val hash : t -> int

val to_array : t -> bool array