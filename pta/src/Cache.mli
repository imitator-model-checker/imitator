open LinearConstraint
open Automaton

type ('a, 'b) t

(** construct a cache given a hash function and the maximum size *)
val make: ('a -> int) -> int -> ('a, 'b) t

(** remove all entries *)
val flush: ('a, 'b) t -> unit

(** change the size of the cache, flushing all entries *)
val resize: ('a, 'b) t -> int -> unit

(** find an item corresponding to a given key *)
val find: ('a, 'b) t -> 'a -> 'b option

(** store an item associated with a key *)
val store: ('a, 'b) t -> 'a -> 'b -> unit

(** print statistics *)
val print_stats: ('a, 'b) t -> unit
