open Format

type verbosity =
  | Silent | Experiments | Normal | Debug

type msg_level =
  | Experiments | Normal | Debug | Always

type t = {
  verbosity : verbosity;
  formatter : formatter; 
  mutable indentation : int;
  mutable live : bool;
}


let create ?(verbosity : verbosity = Normal) ?(formatter=std_formatter) () = { verbosity; formatter; indentation = 0; live = false }

let start_live t = fprintf t.formatter "@."; t.live <- true
let end_live t = t.live <- false

let should_print ~min_level ~verbosity =
  match verbosity, min_level with
  | _, Always -> true (* Always-level messages always print *)
  | Silent, _ -> false
  | Experiments, (Normal | Debug) -> false
  | Normal, Debug -> false

  | _ -> true

let indent fmt indent  = 
  fprintf fmt "%s" (String.concat "" @@ List.init indent (fun _ -> "  "))

  
let msg t ~level format_string =
  if should_print ~min_level:level ~verbosity:t.verbosity then begin
    if t.live then
      fprintf t.formatter "\x1b[A\r\x1b[2K";
    fprintf t.formatter ("%a" ^^ format_string ^^ "@.") indent (t.indentation)
  end
  else
    ifprintf t.formatter format_string

let info t = msg t ~level:Normal
let debug t = msg t ~level:Debug
let warn t = msg t ~level:Experiments
let error t = msg t ~level:Always

let start_section t (title : ('a, formatter, unit, unit) format4) =
  Fun.protect ~finally:(fun () -> t.indentation <- t.indentation + 1) @@
  fun () -> info t ("→ " ^^ title)

let end_section t =
  if should_print ~min_level:Normal ~verbosity:t.verbosity then
    t.indentation <- t.indentation - 1

let flush = Format.print_flush

let with_section t title f = 
  start_section t title;
  Fun.protect ~finally:(fun () -> end_section t) f