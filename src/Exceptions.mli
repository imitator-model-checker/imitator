(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: all public exceptions used in IMITATOR
 * 
 * File contributors : Étienne André
 * Created           : 2014/10/24
 * Last modified     : 2016/10/10
 *
 ************************************************************)


(****************************************************************)
(** Exceptions *)
(****************************************************************)
exception InternalError of string
exception NotImplemented of string
exception SerializationError of string
exception Found
exception InvalidModel
exception UnexpectedToken of char
(** Exception when interfacing with another program *)
exception InterfacingError of string

(** When a constraint contains no integer point *)
exception NoIPPTA

(** Parsing exception: starting position of the error symbol, ending position of the error symbol *)
exception ParsingError of (int * int)

(** When a PTA is not an L/U-PTA *)
exception Not_LU

(** Exception when a problem occurs in NumConst with the random generator initialization *)
exception Random_generator_initialization_exception

(* When trying to get the value of an 'a option that is in fact None *)
exception NoneException
