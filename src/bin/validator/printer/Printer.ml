open Format

type verbosity =
  | Silent | Experiments | Normal | Debug | Always

type t = {
  verbosity : verbosity;
  fmt : formatter; 
}

let create ?(verbosity=Normal) ?(fmt=std_formatter) () = { verbosity; fmt }

let should_print ~min_level ~verbosity =
  match verbosity, min_level with
  | Always, _ -> true
  | Silent, _ -> false
  | Experiments, (Normal | Debug) -> false
  | Normal, Debug -> false

  | _ -> true

let msg t ~level fmt =
  if should_print ~min_level:level ~verbosity:t.verbosity then
    fprintf t.fmt fmt
  else
    ifprintf t.fmt fmt  (* noop formatter *)

let info t fmt = msg t ~level:Normal fmt
let debug t fmt = msg t ~level:Debug fmt
let warn t fmt = msg t ~level:Experiments fmt
let error t fmt = msg t ~level:Always fmt


let start_section t title =
  if should_print ~min_level:Normal ~verbosity:t.verbosity then
    fprintf t.fmt "@[<v 2>→ %s@," title

let end_section t =
  if should_print ~min_level:Normal ~verbosity:t.verbosity then
    fprintf t.fmt "@]@."