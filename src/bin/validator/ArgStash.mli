(** ArgStash — capture selected CLI flags early, retrieve them later.
 
  ArgStash lets you _stash_ specific command-line flags so they don’t
  interfere with other argument parsers (e.g. Crowbar). After stashing,
  those flags are removed from [Sys.argv], and you can retrieve their
  values later by flag name.

  - Supports both ["--flag VALUE"] and ["--flag=VALUE"] forms.
  - If a flag appears multiple times, the **last occurrence wins**.
  - Call {!stash} as early as possible in [main] (before any other
    argument parsing) to avoid clashes.
  - Implementation detail: to ensure a clean [Sys.argv] for downstream
    parsers, the process may replace its image during {!stash}. Treat
    {!stash} as a "run-once, early" function.
*)

(** [stash ~names] captures all occurrences of the given [names] from
    [Sys.argv], removes them, and makes their values available via {!get},
    {!get_or}, and {!get_exn}.

    {b Parsing rules}
    - Matches flags exactly as provided in [names] (e.g. ["--foo"]).
    - Accepts both ["--foo BAR"] and ["--foo=BAR"].
    - If a named flag is present without a value (e.g. trailing ["--foo"]),
      it is ignored.
    - If a flag appears multiple times, the last value wins.

    {b Usage}
    Call once, near program start, before other parsers inspect [Sys.argv].

    @param names The list of flag names to capture (e.g. [["--tool-file"; "--mode"]]).
*)
val stash : names:string list -> unit

(** [get flag] returns the stashed value for [flag] if present.

    Example:
    {[
      let file = ArgStash.get "--tool-file"
    ]}
*)
val get : string -> string option

(** [get_exn flag] returns the stashed value for [flag] or raises if absent.

    @raise Not_found if [flag] was not provided to {!stash} or not present
    on the command line.
*)
val get_exn : string -> string

(** [get_or ~default flag] returns the stashed value for [flag] if present,
    otherwise [default].

    Example:
    {[
      let mode = ArgStash.get_or "--mode" ~default:"quick"
    ]}
*)
val get_or : default:string -> string -> string


(** [stash_and_retrieve arg_list] removes arguments in [arg_list] from [Sys.argv] 
    and returns them as a parsable string array
    
    The optional flag [?include_binary] is [true] by default. It will include the binary itself
    in the 0th index of the returned array to allow easy argument parsing.

    May restart program, so should be called near program start
*)
val stash_and_retrieve : ?include_binary:bool -> string list -> string array 