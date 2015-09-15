(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre
 * 
 * Created:       2014/10/24
 * Last modified: 2015/09/15
 *
 ****************************************************************)


(****************************************************************)
(** Exceptions *)
(****************************************************************)
exception InternalError of string
exception SerializationError of string
exception Found
exception InvalidModel
exception UnexpectedToken of char

(** When a constraint contains no integer point *)
exception NoIPPTA

(** Parsing exception: starting position of the error symbol, ending position of the error symbol *)
exception ParsingError of (int * int)

(** When a PTA is not an L/U-PTA *)
exception Not_LU

