(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: all public exceptions used in IMITATOR
 * 
 * File contributors : Étienne André
 * Created           : 2014/10/24
 *
 ************************************************************)


(****************************************************************)
(** Exceptions *)
(****************************************************************)

(* For constraint conversion *)
exception False_exception

(** Exception raised when a division by 0 is found *)
exception Division_by_0 of string

(** Exception raised when trying an out of bound access to an array or list *)
exception Out_of_range of string

(** Exception raised when trying to access an element on an empty collection *)
exception Empty_collection of string

exception Found

(** Exception when an #include fails in a model *)
exception IncludeFileNotFound of string

(** Exception when interfacing with another program *)
exception InterfacingError of string

exception InvalidModel

exception InternalError of string

(** When the model syntax is too large to apply extrapolation *)
exception Model_not_compatible_for_extrapolation

(** When a constraint contains no integer point *)
exception NoIPPTA

(* When trying to get the value of an 'a option that is in fact None *)
exception NoneException

(** When a PTA is not an L/U-PTA *)
exception Not_LU

exception NotImplemented of string

(** Parsing exception: starting position of the error symbol, ending position of the error symbol *)
exception ParsingError of (int * int)

(** Exception when a problem occurs in NumConst with the random generator initialization *)
exception Random_generator_initialization_exception

exception SerializationError of string

(* Used when an algorithm must suddenly stop (e.g., because it found a certain sort of state) *)
exception TerminateAnalysis

exception UnexpectedToken of char

exception UnsatisfiableInitialState

(* Used when an expression is not well formed *)
exception InvalidExpression of string

(* Used when an expression is ill-typed formed or when computing on not compatible types *)
exception TypeError of string

(* Used when a function isn't found *)
exception UndefinedFunction of string