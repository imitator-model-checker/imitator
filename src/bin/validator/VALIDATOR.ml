open Crowbar

(* A dummy property: reversing a list twice gives the original list *)
let () =
  add_test ~name:"reverse_twice" [list int] (fun xs ->
    let twice = List.rev (List.rev xs) in
    if xs <> twice then
      failf "List.rev (List.rev xs) <> xs for %a"
        (pp_list pp_int) xs
  )
