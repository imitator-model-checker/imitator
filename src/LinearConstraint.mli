(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: common definitions for linear terms and constraints (interface to PPL)
 * 
 * File contributors : Étienne André
 * Created           : 2010/03/04
 * Last modified     : 2019/06/04
 *
 ************************************************************)

 

(************************************************************)
(************************************************************)
(* Exceptions *)
(************************************************************)
(************************************************************)
(* Raised when a linear_term is not a clock guard, i.e., of the form x ~ plterm *)
exception Not_a_clock_guard

(* Raised when a linear_term is not a one-dimensional single parameter constraint, i.e., of the form p ~ c *)
exception Not_a_1d_parameter_constraint

(* Raised when trying to get a point in an empty (false) constraint *)
exception EmptyConstraint


(************************************************************)
(** {2 Variables and coefficients} *)
(************************************************************)

type variable = int
type coef = NumConst.t


(*(** Add on for TA2CLP *)
val string_of_var : (variable -> string) -> variable -> string*)


(************************************************************)
(** {2 Linear terms} *)
(************************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Type} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(* type linear_term *)
type p_linear_term
(* type px_linear_term *)
type pxd_linear_term


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(* val normalize_linear_term : linear_term -> Ppl_ocaml.linear_expression * NumConst.t *)

(** Create a linear term using a list of coef and variables, and a constant *)
(* val make_linear_term : (coef * variable) list -> coef -> linear_term *)
val make_p_linear_term : (coef * variable) list -> coef -> p_linear_term
val make_pxd_linear_term : (coef * variable) list -> coef -> pxd_linear_term


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Add two linear terms *)
(* val add_linear_terms : linear_term -> linear_term -> linear_term *)
val add_pxd_linear_terms : pxd_linear_term -> pxd_linear_term -> pxd_linear_term


(** Perform linear_term1 - linear_term2 *)
(* val sub_linear_terms : linear_term -> linear_term -> linear_term *)

val sub_p_linear_terms : p_linear_term -> p_linear_term -> p_linear_term
(*val sub_px_linear_terms : px_linear_term -> px_linear_term -> px_linear_term*)
val sub_pxd_linear_terms : pxd_linear_term -> pxd_linear_term -> pxd_linear_term

(** Evaluate a linear term with a function assigning a value to each variable. *)
val evaluate_p_linear_term : (variable -> coef) -> p_linear_term -> coef
val evaluate_pxd_linear_term : (variable -> coef) -> pxd_linear_term -> coef


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Convert a linear term into a string *)
(* val string_of_linear_term : (variable -> string) -> linear_term -> string *)
val string_of_p_linear_term : (variable -> string) -> p_linear_term -> string
val string_of_pxd_linear_term : (variable -> string) -> pxd_linear_term -> string


(************************************************************)
(** {2 Linear inequalities} *)
(************************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Type} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
type op =
	| Op_g
	| Op_ge
	| Op_eq
	| Op_le
	| Op_l

(** Reverse an operator: <= becomes >= and conversely. < becomes > and conversely. = remains =. *)
val reverse_op : op -> op

(* Convert an op to string *)
val string_of_op : op -> string

(* type linear_inequality *)
type p_linear_inequality
type px_linear_inequality
type pxd_linear_inequality


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Create a linear inequality using linear term and an operator *)
(* val make_linear_inequality : linear_term -> op -> linear_inequality *)
val make_p_linear_inequality : p_linear_term -> op -> p_linear_inequality
val make_pxd_linear_inequality : pxd_linear_term -> op -> pxd_linear_inequality


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Convert to a nonstrict inequality *)
(* val strict_to_not_strict_inequality : linear_inequality -> linear_inequality *)


(** Check if a linear inequality is pi0-compatible *)
(* val is_pi0_compatible_inequality : (variable -> coef) -> linear_inequality -> bool *)

(** Negate a linear inequality; for an equality, perform the pi0-compatible negation *)
val negate_wrt_pi0 : (variable -> coef) -> p_linear_inequality -> p_linear_inequality

(** Negate an inequality ('=' is disallowed); raises InternalError if "=" is used *)
val negate_inequality : p_linear_inequality -> p_linear_inequality


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)


(*------------------------------------------------------------*)
(** Convert a linear inequality into a clock guard (i.e. a triple clock, operator, parametric linear term); raises Not_a_clock_guard if the linear_inequality is not a proper clock guard x ~ plterm *)
(*------------------------------------------------------------*)
val clock_guard_of_linear_inequality : pxd_linear_inequality -> (Automaton.clock_index * op * p_linear_term)



(** Convert a linear inequality into a string *)
(* val string_of_linear_inequality : (variable -> string) -> linear_inequality -> string *)
val string_of_pxd_linear_inequality : (variable -> string) -> pxd_linear_inequality -> string
val string_of_p_linear_inequality : (variable -> string) -> p_linear_inequality -> string



(************************************************************)
(** {2 Linear Constraints} *)
(************************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Type} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Convex constraint (polyhedron) on the parameters *)
type p_linear_constraint

(** Convex constraint (polyhedron) on the parameters and clocks *)
type px_linear_constraint

(** Convex constraint (polyhedron) on the discrete variables *)
type d_linear_constraint

(** Convex constraint (polyhedron) on the parameters, clocks and discrete *)
type pxd_linear_constraint


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Initialization} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Set the number of dimensions *)
val set_dimensions : int -> int -> int -> unit



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Create a linear constraint from a list of linear inequalities *)
(* val make : linear_inequality list -> linear_constraint *)
val make_p_constraint : p_linear_inequality list -> p_linear_constraint
val make_px_constraint : px_linear_inequality list -> px_linear_constraint
val make_pxd_constraint : pxd_linear_inequality list -> pxd_linear_constraint

(** Create a linear constraint from a single point *)
val p_constraint_of_point : (variable * coef) list -> p_linear_constraint
val pxd_constraint_of_point : (variable * coef) list -> pxd_linear_constraint

(** Create a false constraint *)
(* val false_constraint : unit -> linear_constraint *)
val p_false_constraint : unit -> p_linear_constraint
val pxd_false_constraint : unit -> pxd_linear_constraint

(** Create a true constraint *)
(* val true_constraint : unit -> linear_constraint *)
val p_true_constraint : unit -> p_linear_constraint
val px_true_constraint : unit -> px_linear_constraint
val pxd_true_constraint : unit -> pxd_linear_constraint

(** "pxd_linear_constraint_of_clock_and_parameters x ~ d neg" will create a linear_constraint x ~ d, with x a clock, d a p_linear_term, and "neg" indicates whether x and d should be kept in this direction or reversed (viz., "x < p1 true" generates "x < p1" whereas "x <= p1+p2 false" generates "x >= p1+p2" *)
val px_linear_constraint_of_clock_and_parameters : variable -> op -> p_linear_term -> bool -> px_linear_constraint
val pxd_linear_constraint_of_clock_and_parameters : variable -> op -> p_linear_term -> bool -> pxd_linear_constraint


(** Create a constraint bounding all variables in the list to non-negative *)
val p_constraint_of_nonnegative_variables : variable list -> p_linear_constraint
val px_constraint_of_nonnegative_variables : variable list -> px_linear_constraint
val pxd_constraint_of_nonnegative_variables : variable list -> pxd_linear_constraint


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Access} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Get the number of inequalities of a constraint *)
val p_nb_inequalities : p_linear_constraint -> int

(** Get the linear inequalities of a constraint *)
val pxd_get_inequalities : pxd_linear_constraint -> pxd_linear_inequality list
val p_get_inequalities : p_linear_constraint -> p_linear_inequality list

(** Return true if the variable is constrained in a linear_constraint *)
val pxd_is_constrained : pxd_linear_constraint -> variable -> bool

(** Return the list of variables from l that are constrained in the constraint *)
(* val find_variables : variable list -> linear_constraint -> variable list *)
val pxd_find_variables : variable list -> pxd_linear_constraint -> variable list


(** Given a list of variables V and a list of linear_constraint, partition the list V into variables appearing only as lower-bound in inequalities, and variables only appearing as upper-bounds in inequalities; raise Not_LU if some variables in V appear as both (or in equalities) *)
val partition_lu : variable list -> pxd_linear_constraint list -> (variable list * variable list)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Operations without modification} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Exhibit a point in a linear_constraint; raise EmptyConstraint if the constraint is empty. *)
(*** NOTE: we try to exhibit in each dimension the minimum, except if no minimum (infimum) in which case we get either the middle between the infimum and the supremum (if any supremum), or the infimum if no supremum; and dually if no infimum. ***)
val p_exhibit_point : p_linear_constraint -> (variable -> coef)
val px_exhibit_point : px_linear_constraint -> (variable -> coef)
val pxd_exhibit_point : pxd_linear_constraint -> (variable -> coef)

(** Given two zones z1 and z2, such that z2 is the successor of z1, and given z a subset of z2, then nnconvex_constraint_zone_predecessor z1 z2 z t nott r computes the zone predecessor of z within z1, given the set t (nott) of variables sensitive (resp. insensitive) to time-elapsing, and r the variables reset between z1 and z2. *)
val px_zone_predecessor : px_linear_constraint -> px_linear_constraint -> px_linear_constraint -> (variable list) -> (variable list) -> (variable list) -> px_linear_constraint


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Tests} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Check if a constraint is false *)
(* val is_false : linear_constraint -> bool *)
val p_is_false : p_linear_constraint -> bool
val pxd_is_false : pxd_linear_constraint -> bool

(** Check if a constraint is true *)
(* val is_true : linear_constraint -> bool *)
val p_is_true : p_linear_constraint -> bool
val pxd_is_true : pxd_linear_constraint -> bool

(** Check if a constraint is satisfiable *)
(* val is_satisfiable : linear_constraint -> bool *)
val p_is_satisfiable : p_linear_constraint -> bool
val px_is_satisfiable : px_linear_constraint -> bool
val pxd_is_satisfiable : pxd_linear_constraint -> bool

(** Check if 2 constraints are equal *)
(* val is_equal : linear_constraint -> linear_constraint -> bool *)
val p_is_equal : p_linear_constraint -> p_linear_constraint -> bool
val px_is_equal : px_linear_constraint -> px_linear_constraint -> bool
val pxd_is_equal : pxd_linear_constraint -> pxd_linear_constraint -> bool

(** Check if a constraint is included in another one *)
val p_is_leq : p_linear_constraint -> p_linear_constraint -> bool
val px_is_leq : px_linear_constraint -> px_linear_constraint -> bool
val pxd_is_leq : pxd_linear_constraint -> pxd_linear_constraint -> bool

(** Check if a variable is bound to be >= 0 in a constraint *)
val px_is_positive_in : variable -> px_linear_constraint -> bool

(** Check if a variable is bound to be = 0 in a constraint *)
val px_is_zero_in : variable -> px_linear_constraint -> bool

(** Check if a variable is bound to be = 0 in a constraint *)
val pxd_is_zero_in : variable -> pxd_linear_constraint -> bool

(** Check if a variable is bounded from above in a constraint *)
(*** WARNING: in an equality x <= p, x is NOT considered to be bounded (when p is not itself) ***)
val px_is_bounded_from_above_in : variable -> px_linear_constraint -> bool

(** Check if a variable is bounded from above in a constraint *)
(*** WARNING: in an equality x <= p, x is NOT considered to be bounded (when p is not itself) ***)
val pxd_is_bounded_from_above_in : variable -> pxd_linear_constraint -> bool

(*------------------------------------------------------------*)
(** Return the parametric linear term which is the upper bound of the clock x in a px_linear_constraint; return None if no upper bound *)
(*** NOTE: we asssume that all inequalities are of the form x \sim plt, and that a single inequality constrains x as an upper bound. Raises Not_a_clock_guard otherwise ***)
(*------------------------------------------------------------*)
val clock_upper_bound_in : Automaton.clock_index -> px_linear_constraint -> p_linear_term option


(** Check if a constraint contains an integer point *)
val px_contains_integer_point : px_linear_constraint -> bool

(*------------------------------------------------------------*)
(** Convert a one-dimensional single parameter linear constraint into a single parameter constraint (i.e. a triple parameter_index, operator, constant); raises Not_a_1d_parameter_constraint if the constraint is not a proper constraint *)
(*------------------------------------------------------------*)
val parameter_constraint_of_p_linear_constraint : Automaton.parameter_index -> p_linear_constraint -> (Automaton.parameter_index * op * coef)



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** makes a copy of a constraint *)
(* val copy : linear_constraint -> linear_constraint *)
val p_copy : p_linear_constraint -> p_linear_constraint
val px_copy : px_linear_constraint -> px_linear_constraint
val pxd_copy : pxd_linear_constraint -> pxd_linear_constraint

(** Perform difference (version with side effect) *)
(* val difference_assign : linear_constraint -> linear_constraint -> unit *)

(** Performs the intersection of a list of linear constraints *)
(* val intersection : linear_constraint list -> linear_constraint *)
val p_intersection : p_linear_constraint list -> p_linear_constraint
val px_intersection : px_linear_constraint list -> px_linear_constraint
val pxd_intersection : pxd_linear_constraint list -> pxd_linear_constraint
val pxd_intersection_with_d : pxd_linear_constraint -> d_linear_constraint -> pxd_linear_constraint

(** Performs the intersection of a list of linear constraints with sideeffect *)
(* val intersection_assign : linear_constraint -> linear_constraint list -> unit *)
val p_intersection_assign : p_linear_constraint -> p_linear_constraint list -> unit
val px_intersection_assign : px_linear_constraint -> px_linear_constraint list -> unit
val pxd_intersection_assign : pxd_linear_constraint -> pxd_linear_constraint list -> unit

val px_intersection_assign_p : px_linear_constraint -> p_linear_constraint list -> unit

(** Perform the hull assignation *)
(* val hull_assign : linear_constraint -> linear_constraint -> unit *)

(** Perform convex hull, if the result is exact  *)
(* val hull_assign_if_exact : linear_constraint -> linear_constraint -> bool *)
val px_hull_assign_if_exact : px_linear_constraint -> px_linear_constraint -> bool

(** Eliminate (using existential quantification) all non-parameters (clocks) in a px_linear constraint *)
val px_hide_nonparameters_and_collapse : px_linear_constraint -> p_linear_constraint

(** Eliminate (using existential quantification) all non-parameters (clocks only, as it is a PX constraint) and some parameters in a px_linear constraint *)
val px_hide_allclocks_and_someparameters_and_collapse : variable list -> px_linear_constraint -> p_linear_constraint

(** Eliminate (using existential quantification) the discrete variables in a pxd_linear constraint, and remove the corresponding dimensions *)
val pxd_hide_discrete_and_collapse : pxd_linear_constraint -> px_linear_constraint

(** Eliminate (using existential quantification) the non-parameters in a pxd_linear constraint, and remove the corresponding dimensions *)
(* val pxd_hide_nonparameters_and_collapse : pxd_linear_constraint -> p_linear_constraint *)

(*------------------------------------------------------------*)
(* Convex negation *)
(*------------------------------------------------------------*)
(** Assuming p_linear_constraint contains a single inequality, this function returns the negation of this inequality (in the form of a p_constraint). Raises InternalError if more than one inequality. *)
val negate_single_inequality_p_constraint : p_linear_constraint -> p_linear_constraint

(** Negates a constraint made either of a single inequality, or made of 2 inequalities, one of which is p >= 0, for a given p *)
(*** HACK: a very ad-hoc function, needed for EFmax ***)
val negate_single_inequality_nonnegative_p_constraint : Automaton.parameter_index -> p_linear_constraint -> p_linear_constraint


(** Eliminate (using existential quantification) a set of variables in a linear constraint, with side effects *)
val p_hide_assign : variable list -> p_linear_constraint -> unit
val px_hide_assign : variable list -> px_linear_constraint -> unit
val pxd_hide_assign : variable list -> pxd_linear_constraint -> unit

(** Eliminate (using existential quantification) a set of variables in a linear constraint, without side effects *)
val p_hide : variable list -> p_linear_constraint -> p_linear_constraint

(** Add nb_dimensions to a linear_constraint *)
(* val add_dimensions : int -> linear_constraint -> unit *)
val pxd_add_dimensions : int -> pxd_linear_constraint -> unit

(** Remove the highest nb_dimensions from a linear_constraint *)
(* val remove_dimensions : int -> linear_constraint -> unit *)
val pxd_remove_dimensions : int -> pxd_linear_constraint -> unit

(** 'rename_variables renaming_couples c' renames all variables according to the couples of the form (old, new) *)
(* val rename_variables : (variable * variable) list -> linear_constraint -> linear_constraint *)
(* val rename_variables : (variable * variable) list -> pxd_linear_constraint -> pxd_linear_constraint *)

(** 'rename_variables renaming_couples c' renames all variables according to the couples of the form (old, new), with side effects *)
(* val rename_variables_assign : (variable * variable) list -> linear_constraint -> unit *)
val pxd_rename_variables_assign : (variable * variable) list -> pxd_linear_constraint -> unit

(** 'time_elapse_assign variables_elapse variables_constant linear_constraint' performs time elapsing on a set of variables variables_elapse; other variables remain constant; version with side effects; behavior is unspecified if some variables within linear_constraint do not appear in any set of variables *)
(* val time_elapse_assign : variable list -> variable list -> linear_constraint -> unit *)
val pxd_time_elapse_assign : variable list -> variable list -> pxd_linear_constraint -> unit

(** Time elapsing function, in backward direction (corresponds to the "past" operation in, e.g., [JLR15]) *)
val pxd_time_past_assign : variable list -> variable list -> pxd_linear_constraint -> unit

(** Remove all upper bounds on the first variable list; the second list will remain constant *)
(*** WARNING: this function is certainly not optimized at all! ***)
(*** WARNING: the behavior is unspecified if some variables belong to no list, or to both lists ***)
val p_grow_to_infinity_assign : variable list -> variable list -> p_linear_constraint -> unit

(** Remove all lower bounds on the first variable list; the second list will remain constant *)
(** WARNING: this function is certainly not optimized at all! *)
(*** WARNING: the behavior is unspecified if some variables belong to no list, or to both lists ***)
val p_grow_to_zero_assign : variable list -> variable list -> p_linear_constraint -> unit


(** Replace all strict inequalities with non-strict (and keeps others unchanged) within a p_linear_constraint *)
val render_non_strict_p_linear_constraint : p_linear_constraint -> p_linear_constraint



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Pi0-compatibility} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Check if a p_linear_constraint is pi0-compatible, i.e., whether the parameter valuation satisfies the linear constraint *)
val is_pi0_compatible : (variable -> coef) -> p_linear_constraint -> bool

(** Check if a d_linear_constraint is pi0-compatible, i.e., whether the discrete valuation satisfies the linear constraint *)
val d_is_pi0_compatible : (variable -> coef) -> d_linear_constraint -> bool


(** Compute the pi0-compatible and pi0-incompatible inequalities within a constraint *)
val partition_pi0_compatible : (variable -> coef) -> p_linear_constraint -> (p_linear_inequality list * p_linear_inequality list)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Conversion between types of constraints } *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** Create a pxd_linear_constraint from a set of pairs (discrete variable, value) *)
val pxd_constraint_of_discrete_values : (variable * coef) list -> pxd_linear_constraint

(** Convert (and copy) a PX into a PXD constraint by extending the number of dimensions; the original constraint remains unchanged *)
val px_of_p_constraint : p_linear_constraint -> px_linear_constraint
val pxd_of_p_constraint : p_linear_constraint -> pxd_linear_constraint
val pxd_of_px_constraint : px_linear_constraint -> pxd_linear_constraint


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Brute-force casts (argh) } *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** "cast_p_of_pxd_linear_term p c" converts a PXD-term p to a P-term ; if c then a test if performed to check casting validity *)
(*** WARNING: in fact, for now NO TEST IS EVER PERFORMED ***)
val cast_p_of_pxd_linear_term : pxd_linear_term -> bool -> p_linear_term
val cast_p_of_pxd_linear_constraint : pxd_linear_constraint -> bool -> p_linear_constraint

val cast_d_of_pxd_linear_constraint : bool -> pxd_linear_constraint -> d_linear_constraint



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Conversion to string} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Convert a linear constraint into a string using default values *)
val string_of_p_linear_constraint : (variable -> string) -> p_linear_constraint -> string
val string_of_px_linear_constraint : (variable -> string) -> px_linear_constraint -> string
val string_of_d_linear_constraint : (variable -> string) -> d_linear_constraint -> string
val string_of_pxd_linear_constraint : (variable -> string) -> pxd_linear_constraint -> string

(** Data structure allowing for customizing string conversions *)
type customized_string = {
	true_string  : string;
	false_string : string;
	and_operator : string;
	or_operator  : string;
	l_operator   : string;
	le_operator  : string;
	eq_operator  : string;
	ge_operator  : string;
	g_operator   : string;
}

(** Convert a linear constraint into a string using personalized values *)
val customized_string_of_p_linear_constraint : customized_string -> (variable -> string) -> p_linear_constraint -> string
val customized_string_of_px_linear_constraint : customized_string -> (variable -> string) -> px_linear_constraint -> string
val customized_string_of_d_linear_constraint : customized_string -> (variable -> string) -> d_linear_constraint -> string
val customized_string_of_pxd_linear_constraint : customized_string -> (variable -> string) -> pxd_linear_constraint -> string

(** String for the false constraint *)
val string_of_false : string

(** String for the true constraint *)
val string_of_true : string

(** String for the intersection symbol *)
val string_of_and : string


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Conversion to GrML} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(*(** Convert a linear term into a string for GML *)
val grml_of_pxd_linear_term : (variable -> string) -> int -> pxd_linear_term -> string

(** Convert a linear constraint into a string for GML *)
val grml_of_px_linear_constraint : (variable -> string) -> int -> px_linear_constraint -> string
val grml_of_pxd_linear_constraint : (variable -> string) -> int -> pxd_linear_constraint -> string*)



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Conversion to plot} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** converts a linear_constraint to a set of 2d points wrt. the variables x,y *)
val shape_of_poly : variable -> variable -> p_linear_constraint -> (float*float) list *(float*float) list

(** Plot polyhedron corresponding to a convex constraint, projected on the two given variables *)
val plot_2d : variable -> variable -> p_linear_constraint -> float -> float -> float -> float -> bool*string






(************************************************************)
(** {2 Non-necessarily convex linear Constraints} *)
(************************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Type} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Non-necessarily convex constraint on the parameters ("pointset powerset" in the underlying PPL implementation) *)
type p_nnconvex_constraint
type px_nnconvex_constraint


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Create a false constraint *)
val false_p_nnconvex_constraint : unit -> p_nnconvex_constraint
val false_px_nnconvex_constraint : unit -> px_nnconvex_constraint

(** Create a true constraint *)
val true_p_nnconvex_constraint  : unit -> p_nnconvex_constraint
val true_px_nnconvex_constraint  : unit -> px_nnconvex_constraint

(** Copy a p_nnconvex_constraint *)
val p_nnconvex_copy : p_nnconvex_constraint -> p_nnconvex_constraint
(** Copy a px_nnconvex_constraint *)
val px_nnconvex_copy : px_nnconvex_constraint -> px_nnconvex_constraint


(** Create a new p_nnconvex_constraint from a linear_constraint *)
val p_nnconvex_constraint_of_p_linear_constraint : p_linear_constraint -> p_nnconvex_constraint
val px_nnconvex_constraint_of_px_linear_constraint : px_linear_constraint -> px_nnconvex_constraint

(** Create a new non-convex p_nnconvex_constraint from a list of linear_constraint *)
val p_nnconvex_constraint_of_p_linear_constraints : p_linear_constraint list -> p_nnconvex_constraint


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Tests} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Check if a nnconvex_constraint is false *)
val p_nnconvex_constraint_is_false : p_nnconvex_constraint -> bool

(** Check if a nnconvex_constraint is true *)
val p_nnconvex_constraint_is_true  : p_nnconvex_constraint -> bool

(** Check if a nnconvex_constraint is pi0-compatible *)
val p_nnconvex_constraint_is_pi0_compatible : (variable -> coef) -> p_nnconvex_constraint -> bool

(** Check if a nnconvex_constraint is included in another one *)
val p_nnconvex_constraint_is_leq : p_nnconvex_constraint -> p_nnconvex_constraint -> bool

(** Check if a nnconvex_constraint is equal to another one *)
val p_nnconvex_constraint_is_equal : p_nnconvex_constraint -> p_nnconvex_constraint -> bool


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Modifications} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Performs the intersection of a p_nnconvex_constraint with a p_linear_constraint; the p_nnconvex_constraint is modified, the p_linear_constraint is not *)
val p_nnconvex_intersection  : p_nnconvex_constraint -> p_linear_constraint -> unit
val px_nnconvex_intersection  : px_nnconvex_constraint -> px_linear_constraint -> unit

(** Performs the union of a p_nnconvex_constraint with a p_linear_constraint; the p_nnconvex_constraint is modified, the p_linear_constraint is not *)
val p_nnconvex_p_union : p_nnconvex_constraint -> p_linear_constraint -> unit
val px_nnconvex_px_union : px_nnconvex_constraint -> px_linear_constraint -> unit

(** Performs the union of a p_nnconvex_constraint with another p_nnconvex_constraint; the first p_nnconvex_constraint is modified, the second is not *)
val p_nnconvex_union : p_nnconvex_constraint -> p_nnconvex_constraint -> unit

(** Performs the difference between a first px_nnconvex_constraint and a second px_nnconvex_constraint; the first is modified, the second is not *)
val p_nnconvex_difference : p_nnconvex_constraint -> p_nnconvex_constraint -> unit
val px_nnconvex_difference : px_nnconvex_constraint -> px_nnconvex_constraint -> unit

(** Eliminate a set of variables *)
val p_nnconvex_hide : variable list -> p_nnconvex_constraint -> p_nnconvex_constraint
val px_nnconvex_hide : variable list -> px_nnconvex_constraint -> px_nnconvex_constraint

(** Eliminate (using existential quantification) all non-parameters (clocks) in a px_linear constraint *)
val px_nnconvex_hide_nonparameters_and_collapse : px_nnconvex_constraint -> p_nnconvex_constraint


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Conversion to a list of p_linear_constraint} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** Converts a p_nnconvex_constraint into a list of p_linear_constraint such that the union of this list is equal to the p_nnconvex_constraint *)
val p_linear_constraint_list_of_p_nnconvex_constraint : p_nnconvex_constraint -> p_linear_constraint list


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Conversion to string} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Convert a p_nnconvex_constraint into a string *)
val string_of_p_nnconvex_constraint : (variable -> string) -> p_nnconvex_constraint -> string
val string_of_px_nnconvex_constraint : (variable -> string) -> px_nnconvex_constraint -> string



(************************************************************)
(** {2 Non-necessarily convex linear Constraints} *)
(************************************************************)
type p_convex_or_nonconvex_constraint =
	| Convex_p_constraint of p_linear_constraint
	| Nonconvex_p_constraint of p_nnconvex_constraint

(** Convert a p_convex_or_nonconvex_constraint into a string *)
val string_of_p_convex_or_nonconvex_constraint : (variable -> string) -> p_convex_or_nonconvex_constraint -> string




(************************************************************)
(** {2 Serialization} *)
(************************************************************)
val serialize_variable : variable -> string
val unserialize_variable : string -> variable

(*val serialize_p_linear_constraint : p_linear_constraint -> string
val unserialize_p_linear_constraint : string -> p_linear_constraint*)

val serialize_p_nnconvex_constraint : p_nnconvex_constraint -> string
val unserialize_p_nnconvex_constraint : string -> p_nnconvex_constraint

val serialize_p_convex_or_nonconvex_constraint : p_convex_or_nonconvex_constraint -> string
val unserialize_p_convex_or_nonconvex_constraint : string -> p_convex_or_nonconvex_constraint


(************************************************************)
(** {2 Statistics on performances} *)
(************************************************************)
(* val get_statistics : float -> string *)


(************************************************************)
(** {2 Tests} *)
(************************************************************)
val test_PDBMs : unit -> unit



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** Gia's function for CUB **)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(*for linear term*)
val operator2string : op -> string

(*
val get_coefs_vars : p_linear_term -> (variable*coef) list
*)

(*
type smaller_term =
	| NotDetermine (*not determined*)
	| First
	| Second

(*for linear term*)
val isComparable_linear_terms : p_linear_term -> p_linear_term -> (bool*smaller_term)

val isComparable_linear_terms_2 : p_linear_term -> p_linear_term -> p_linear_constraint
(* val get_coefficient_in_linear_term : Ppl.linear_expression -> NumConst.t   *) 

*)



type smaller_term =
	| NotDetermine (*not determined*)
	| First
	| Second

(*for linear term*)
(*val isSmaller : p_linear_term -> p_linear_term -> (bool*smaller_term)*)
val isSmaller : p_linear_term -> p_linear_term -> smaller_term

(*
val isComparable_linear_terms : p_linear_term -> p_linear_term -> (bool*smaller_term)
*)

(*
val isComparable_linear_terms_2 : p_linear_term -> p_linear_term -> p_linear_constraint
(* val get_coefficient_in_linear_term : Ppl.linear_expression -> NumConst.t   *) 
*)


