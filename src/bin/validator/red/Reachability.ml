open Lib
open ParsingStructure

let adjacency_matrix_of_parsed_locations parsed_locations =
  let i_of_name =
    let tbl = Hashtbl.create 32 in
    List.iteri (fun i loc -> Hashtbl.add tbl loc.name i) parsed_locations;
    Hashtbl.find tbl
  in
  let n = List.length parsed_locations in
  let adj = Array.make_matrix n n false in
  List.iteri (fun i loc ->
    List.iter (fun (_, _, _, dst) ->
      adj.(i).(i_of_name dst) <- true
    ) loc.transitions
  ) parsed_locations;
  adj

let remove_islands_from_automaton (names, actions, locations) =
  let adj = adjacency_matrix_of_parsed_locations locations in
  let n = Array.length adj in
  let indeg = Array.make n false in
  let outdeg = Array.make n false in

  Array.iteri (fun i row ->
    Array.iteri (fun j edge ->
      if edge then begin
        outdeg.(i) <- true;
        indeg.(j) <- true
      end
    ) row
  ) adj;

  let locations' =
    List.filteri (fun i _ -> outdeg.(i) || indeg.(i)) locations
  in
  names, actions, locations'

let remove_islands (model : parsed_model) : parsed_model =
  { model with automata = List.map remove_islands_from_automaton model.automata }
