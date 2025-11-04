open Format

type verbosity =
  | Silent | Experiments | Normal | Debug | Always

type t 

val create : ?verbosity:verbosity -> ?fmt:formatter -> unit -> t

val info : t -> ('a, formatter, unit, unit) format4 -> 'a 

val debug : t -> ('a, formatter, unit, unit) format4 -> 'a 

val warn : t -> ('a, formatter, unit, unit) format4 -> 'a 

val error : t -> ('a, formatter, unit, unit) format4 -> 'a

val start_section : t -> string -> unit

val end_section : t -> unit