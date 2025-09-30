open Lib
open ParsingStructure
open Crowbar

let parsed_model : parsed_model gen = 
  map [int] (fun nb_automata -> 
    {
      automata = [];
      controllable_actions = Parsed_no_controllable_actions;
	    variable_declarations = [];
	    fun_definitions = [];
	    init_definition = [];
    }
  )