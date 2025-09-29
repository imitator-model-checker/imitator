


let normalize_env_name (flag : string) : string =
  let s =
    if String.length flag >= 2 && String.sub flag 0 2 = "--"
    then String.sub flag 2 (String.length flag - 2)
    else flag
  in
  String.map (function '-' -> '_' | c -> Char.uppercase_ascii c) s

(* Scan argv for any of [names]; remove them; return (env_updates, filtered_argv). 
    Supports both --name value and --name=value forms. Last one wins. *)
let extract_many (names : string list) :
    (string * string) list * string array =
  let updates = Hashtbl.create 8 in
  let keep = ref [] in
  let i = ref 0 in
  let argv = Sys.argv in
  let n = Array.length argv in
  let is_target_flag s =
    (* match exact name or --name=val form *)
    List.exists
      (fun nm ->
        s = nm ||
        (String.length s > String.length nm + 1 &&
          String.sub s 0 (String.length nm) = nm &&
          String.get s (String.length nm) = '='))
      names
  in
  while !i < n do
    let cur = argv.(!i) in
    if is_target_flag cur then begin
      (* case A: --flag=value *)
      (match List.find_opt (fun nm ->
                String.length cur > String.length nm + 1 &&
                String.sub cur 0 (String.length nm) = nm &&
                String.get cur (String.length nm) = '=')
              names
        with
        | Some nm ->
            let v = String.sub cur (String.length nm + 1)
                                  (String.length cur - (String.length nm + 1)) in
            Hashtbl.replace updates (normalize_env_name nm) v;
            incr i
        | None ->
            (* case B: --flag value *)
            (match List.find_opt ((=) cur) names with
            | Some nm when !i + 1 < n ->
                let v = argv.(!i + 1) in
                Hashtbl.replace updates (normalize_env_name nm) v;
                i := !i + 2
            | _ ->
                (* malformed: flag at end; drop it silently *)
                incr i));
    end else begin
      keep := cur :: !keep;
      incr i
    end
  done;
  let envs =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) updates []
  in
  (envs, Array.of_list (List.rev !keep))

(* stash and export; re-exec only if we actually removed something. *)
let stash ~(names : string list) : unit =
  let envs, kept = extract_many names in
  if envs <> [] then begin
    List.iter (fun (k, v) -> Unix.putenv k v) envs;
    Unix.execvpe kept.(0) kept (Unix.environment ())
  end
  else
    ()

(* Read a stashed arg back from env *)
let get (flag_name : string) : string option =
  Sys.getenv_opt (normalize_env_name flag_name)

let get_exn (flag_name : string) : string =
  match get flag_name with
  | Some v -> v
  | None ->
      raise Not_found

let get_or ~default flag_name =
  match get flag_name with Some v -> v | None -> default

