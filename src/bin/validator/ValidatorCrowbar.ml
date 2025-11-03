open Crowbar

(* redirect stdout/stderr to /dev/null *)
let disable_output () =
  let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0o666 in
  Unix.dup2 devnull Unix.stdout;
  Unix.dup2 devnull Unix.stderr;
  Unix.close devnull

(* reopen stdout/stderr to terminal, if available *)
let enable_output () =
  try
    let tty = Unix.openfile "/dev/tty" [Unix.O_WRONLY] 0o666 in
    Unix.dup2 tty Unix.stdout;
    Unix.dup2 tty Unix.stderr;
    Unix.close tty
  with _ ->
    ()  (* /dev/tty might not exist, e.g. CI or redirection *)


let with_output_enabled f =
  enable_output ();
  Fun.protect ~finally:disable_output f

let add_test gens f =
  Crowbar.add_test gens (fun x ->
    with_output_enabled (fun () -> f x)
  )
