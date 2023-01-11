open ParsedValue
open Exceptions

module ExtInt32 = struct
    include Int32
    let of_parsed_value = function
        | Int_value v -> v
        | _ -> raise (InternalError "")

    let to_parsed_value v = Int_value v

end

module ExtNumConst = struct
    include NumConst
    let of_parsed_value = function
        | Weak_number_value v
        | Rat_value v -> v
        | _ -> raise (InternalError "")

    let to_parsed_value v = Rat_value v

end