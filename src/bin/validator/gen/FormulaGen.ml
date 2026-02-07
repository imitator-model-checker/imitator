open QCheck2
open SimpleModel
open Validator_config
open Gen

let cop_of_sop : Spec.constraint_type -> SimpleModel.cop = function
  | S_EQ -> EQ
  | S_LE -> LEQ
  | S_LT -> L
  | S_GE -> GEQ
  | S_GT -> G

let cops_of_sops (sops : Spec.constraint_type list) : SimpleModel.cop list =
  List.map cop_of_sop sops

let term_constant = 
  let+ n =  small_nat in 
  SConstant n 

let term_variable ~nb_clocks ~nb_parameters = 
  let* coinflip = bool in
  if nb_parameters = 0 || coinflip then 
    let+ sample = int_range 0 (nb_clocks - 1) in 
    SClock sample
  else
    let+ sample = int_range 0 (nb_parameters - 1) in 
    SParam sample

let bool_expr ~nb_clocks ~nb_parameters ~opers = 
  let+ clock = term_variable ~nb_clocks ~nb_parameters
  and+ constant = term_constant
  and+ oper = oneofl opers in 
  SComp (clock, oper, constant)

let formula ~nb_clocks ~nb_parameters ~opers = 
  let opers = cops_of_sops opers in  
  let comparison = bool_expr ~nb_clocks ~nb_parameters ~opers in
  flatten_l [comparison]