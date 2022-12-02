(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: arithmetic and Boolean expressions on discrete variables
 *
 * File contributors : Étienne André, Dylan Marinho
 * Created           : 2019/12/10
 *
 ************************************************************)

type variable_name = string
type variable_name_table = Automaton.variable_index -> variable_name
(************************************************************)
(************************************************************)
(** Operators *)
(************************************************************)
(************************************************************)

(** Boolean operators *)

type relop = OP_L | OP_LEQ | OP_EQ | OP_NEQ | OP_GEQ | OP_G


(************************************************************)
(************************************************************)
(** Valuation *)
(************************************************************)
(************************************************************)

type conj_dis =
    | And
    | Or

type sum_diff =
    | Plus
    | Minus

type product_quotient =
    | Mul
    | Div

type loop_dir =
    | Loop_up
    | Loop_down

type update_scope =
    | Global_update of Automaton.discrete_index
    | Local_update of variable_name

(****************************************************************)
(** Global expression *)
(****************************************************************)
type global_expression =
    (* A typed expression *)
    | Void_expression of void_expression
    | Arithmetic_expression of discrete_arithmetic_expression
    | Bool_expression of boolean_expression
    | Binary_word_expression of binary_word_expression
    | Array_expression of array_expression
    | List_expression of list_expression
    | Stack_expression of stack_expression
    | Queue_expression of queue_expression

and discrete_arithmetic_expression =
    | Rational_arithmetic_expression of rational_arithmetic_expression
    | Int_arithmetic_expression of int_arithmetic_expression

(****************************************************************)
(** Arithmetic expressions for discrete variables *)
(****************************************************************)
and rational_arithmetic_expression =
    | Rational_sum_diff of rational_arithmetic_expression * rational_term * sum_diff
	| Rational_term of rational_term

and rational_term =
	| Rational_product_quotient of rational_term * rational_factor * product_quotient
	| Rational_factor of rational_factor

and rational_factor =
	| Rational_variable of Automaton.variable_index
	| Rational_local_variable of variable_name
	| Rational_constant of NumConst.t
	| Rational_nested_expression of rational_arithmetic_expression
	| Rational_unary_min of rational_factor
	| Rational_pow of rational_arithmetic_expression * int_arithmetic_expression
    | Rational_array_access of expression_access_type * int_arithmetic_expression
    | Rational_function_call of variable_name * variable_name list * global_expression list

(************************************************************)
(** Int arithmetic expressions for discrete variables *)
(************************************************************)
(************************************************************)
and int_arithmetic_expression =
    | Int_sum_diff  of int_arithmetic_expression * int_term * sum_diff
	| Int_term of int_term

and int_term =
	| Int_product_quotient of int_term * int_factor * product_quotient
	| Int_factor of int_factor

and int_factor =
	| Int_variable of Automaton.variable_index
	| Int_local_variable of variable_name
	| Int_constant of Int32.t
	| Int_nested_expression of int_arithmetic_expression
	| Int_unary_min of int_factor
    | Int_pow of int_arithmetic_expression * int_arithmetic_expression
    | Int_array_access of expression_access_type * int_arithmetic_expression
    | Int_function_call of variable_name * variable_name list * global_expression list


(************************************************************)
(************************************************************)
(************************************************************)
(** Boolean expressions for discrete variables *)
(************************************************************)
(************************************************************)

(** Boolean expression *)
and boolean_expression =
	| True_bool (** True *)
	| False_bool (** False *)
    | Conj_dis of boolean_expression * boolean_expression * conj_dis (** Conjunction / Disjunction *)
	| Discrete_boolean_expression of discrete_boolean_expression

and discrete_boolean_expression =
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	(* TODO benjamin REFACTOR create another type regrouping all comparisons *)
    (* TODO benjamin look for Arithmetic_comparison because even if it was type checked before it's structure can potentially compare different type *)
	| Arithmetic_comparison of discrete_arithmetic_expression * relop * discrete_arithmetic_expression
    | Boolean_comparison of discrete_boolean_expression * relop * discrete_boolean_expression
    | Binary_comparison of binary_word_expression * relop * binary_word_expression
    | Array_comparison of array_expression * relop * array_expression
    | List_comparison of list_expression * relop * list_expression
    | Stack_comparison of stack_expression * relop * stack_expression
    | Queue_comparison of queue_expression * relop * queue_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in of discrete_arithmetic_expression * discrete_arithmetic_expression * discrete_arithmetic_expression
	(** Parsed boolean expression of the form Expr ~ Expr, with ~ = { &, | } or not (Expr) *)
	| Boolean_expression of boolean_expression
	(** Parsed boolean expression of the form not(Expr ~ Expr), with ~ = { &, | } *)
	| Not_bool of boolean_expression (** Negation *)
	(** discrete variable in boolean expression *)
	| Bool_variable of Automaton.variable_index
	| Bool_local_variable of variable_name
	(** discrete constant in boolean expression *)
	| Bool_constant of bool
    | Bool_array_access of expression_access_type * int_arithmetic_expression

    | Bool_function_call of variable_name * variable_name list * global_expression list

(************************************************************)
(************************************************************)
(************************************************************)
(** Binary word expressions for discrete variables *)
(************************************************************)
(************************************************************)

(** Binary word expression *)
and binary_word_expression =
    | Binary_word_constant of BinaryWord.t
    | Binary_word_variable of Automaton.variable_index * int
	| Binary_word_local_variable of variable_name
    | Binary_word_array_access of expression_access_type * int_arithmetic_expression
    | Binary_word_function_call of variable_name * variable_name list * global_expression list

(** Array expression **)
and array_expression =
    | Literal_array of global_expression array
    | Array_constant of AbstractValue.abstract_value array
    | Array_variable of Automaton.variable_index
    | Array_local_variable of variable_name
    | Array_array_access of expression_access_type * int_arithmetic_expression
    | Array_function_call of variable_name * variable_name list * global_expression list

(** List expression **)
and list_expression =
    | Literal_list of global_expression list
    | List_constant of AbstractValue.abstract_value list
    | List_variable of Automaton.variable_index
    | List_local_variable of variable_name
    | List_array_access of expression_access_type * int_arithmetic_expression
    | List_function_call of variable_name * variable_name list * global_expression list

and stack_expression =
    | Literal_stack
    | Stack_variable of Automaton.variable_index
    | Stack_local_variable of variable_name
    | Stack_array_access of expression_access_type * int_arithmetic_expression
    | Stack_function_call of variable_name * variable_name list * global_expression list

and queue_expression =
    | Literal_queue
    | Queue_variable of Automaton.variable_index
    | Queue_local_variable of variable_name
    | Queue_array_access of expression_access_type * int_arithmetic_expression
    | Queue_function_call of variable_name * variable_name list * global_expression list

and void_expression =
    | Void_function_call of variable_name * variable_name list * global_expression list

and expression_access_type =
    | Expression_array_access of array_expression
    | Expression_list_access of list_expression

(* Bloc of sequential code *)
and seq_code_bloc =
    | Local_decl of variable_name * DiscreteType.var_type_discrete * global_expression (* init expr *)
    | Assignment of discrete_update
    | Local_assignment of discrete_update
    | Clock_assignment of (Automaton.clock_index * LinearConstraint.pxd_linear_term)
    | Instruction of global_expression
    | For_loop of variable_name * int_arithmetic_expression (* from *) * int_arithmetic_expression (* to *) * loop_dir (* up or down *) * seq_code_bloc_list (* inner bloc *)
    | While_loop of boolean_expression (* condition *) * seq_code_bloc_list (* inner bloc *)
    | If of boolean_expression (* condition *) * seq_code_bloc_list (* then bloc *) * seq_code_bloc_list option (* else bloc *)

and seq_code_bloc_list = seq_code_bloc list

and discrete_update = scalar_or_index_update_type * global_expression

(* Update type *)
and scalar_or_index_update_type =
    (* Variable update, ie: x := 1 *)
    | Scalar_update of update_scope
    (* Indexed element update, ie: x[i] = 1 or x[i][j] = 2 *)
    | Indexed_update of scalar_or_index_update_type * int_arithmetic_expression

type fun_type =
    | Fun_builtin of (string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value)
    | Fun_user of seq_code_bloc_list * global_expression option

type nonlinear_constraint = discrete_boolean_expression list

val is_linear_discrete_boolean_expression : discrete_boolean_expression -> bool
val is_linear_nonlinear_constraint : nonlinear_constraint -> bool

(* String *)

(* Constructors strings *)
val label_of_bool_factor : discrete_boolean_expression -> string
val label_of_rational_factor : rational_factor -> string
val label_of_int_factor : int_factor -> string
val label_of_binary_word_expression : binary_word_expression -> string
val label_of_array_expression : array_expression -> string
val label_of_list_expression : list_expression -> string
val label_of_stack_expression : stack_expression -> string
val label_of_queue_expression : queue_expression -> string

(* String representation of boolean according to customized string *)
val customized_string_of_bool_value : Constants.customized_boolean_string -> bool -> string
(* String representation of boolean operations according to customized string *)
val customized_string_of_boolean_operations : Constants.customized_boolean_string -> relop -> string

(* Expressions strings *)

val customized_string_of_global_expression : Constants.customized_string -> variable_name_table -> global_expression -> string
val string_of_global_expression : variable_name_table -> global_expression -> string

val customized_string_of_arithmetic_expression : Constants.customized_string -> variable_name_table -> discrete_arithmetic_expression -> string
val string_of_arithmetic_expression : variable_name_table -> discrete_arithmetic_expression -> string

val customized_string_of_int_arithmetic_expression : Constants.customized_string -> variable_name_table -> int_arithmetic_expression -> string
val string_of_int_arithmetic_expression : variable_name_table -> int_arithmetic_expression -> string

val customized_string_of_boolean_expression : Constants.customized_string -> variable_name_table -> boolean_expression -> string
val string_of_boolean_expression : variable_name_table -> boolean_expression -> string

val customized_string_of_discrete_boolean_expression : Constants.customized_string -> variable_name_table -> discrete_boolean_expression -> string
val string_of_discrete_boolean_expression : variable_name_table -> discrete_boolean_expression -> string

val customized_string_of_array_expression : Constants.customized_string -> variable_name_table -> array_expression -> string
val string_of_array_expression : variable_name_table -> array_expression -> string

val string_of_list_expression : variable_name_table -> list_expression -> string
val string_of_stack_expression : variable_name_table -> stack_expression -> string
val string_of_queue_expression : variable_name_table -> queue_expression -> string

val customized_string_of_discrete_update : Constants.customized_string -> variable_name_table -> discrete_update -> string
val string_of_discrete_update : variable_name_table -> discrete_update -> string
val customized_string_of_scalar_or_index_update_type : Constants.customized_string -> variable_name_table -> scalar_or_index_update_type -> string
val string_of_scalar_or_index_update_type : variable_name_table -> scalar_or_index_update_type -> string

val string_of_expression_access : variable_name_table -> expression_access_type -> int_arithmetic_expression -> string

val customized_string_of_nonlinear_constraint : Constants.customized_string -> variable_name_table -> nonlinear_constraint -> string
val string_of_nonlinear_constraint : variable_name_table -> nonlinear_constraint -> string