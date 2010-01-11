(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, * CNRS-Ecole Polytechnique-INRIA Futurs-Universite Paris Sud *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(*i $Id$ i*)

open Names
open Term
open Environ
open Evd

val short_version : unit -> string
val version : unit -> string

type printing_state = {
  mutable printing_implicit : bool;
  mutable printing_coercions : bool;
  mutable printing_raw_matching : bool;
  mutable printing_no_notation : bool;
  mutable printing_all : bool;
  mutable printing_evar_instances : bool;
  mutable printing_universes : bool;
  mutable printing_full_all : bool
}

val printing_state : printing_state

type reset_mark =
  | ResetToId of Names.identifier
  | ResetToState of Libnames.object_name

type reset_status =
  | NoReset
  | ResetAtSegmentStart of Names.identifier
  | ResetAtRegisteredObject of reset_mark

type undo_info = identifier list

val undo_info : unit -> undo_info

type reset_info = {
 status : reset_status;
 proofs : undo_info;
 loc_ast : Util.loc * Vernacexpr.vernac_expr;
 mutable section : bool;
}

val compute_reset_info : Util.loc * Vernacexpr.vernac_expr -> reset_info
val reset_initial : unit -> unit
val reset_to : reset_mark -> unit
val reset_to_mod : identifier -> unit

val init : unit -> string list
val interp : bool -> string -> reset_info
val interp_last : Util.loc * Vernacexpr.vernac_expr -> unit
val interp_and_replace : string ->
      reset_info * string

val push_phrase : ('a * reset_info) Stack.t -> reset_info -> 'a -> unit

type undo_cmds
val init_undo_cmds : unit -> undo_cmds
val pop_command : ('a * reset_info) Stack.t -> undo_cmds -> undo_cmds
val apply_undos : ('a * reset_info) Stack.t -> undo_cmds -> unit

val is_vernac_tactic_command : Vernacexpr.vernac_expr -> bool
val is_vernac_state_preserving_command : Vernacexpr.vernac_expr -> bool
val is_vernac_goal_starting_command : Vernacexpr.vernac_expr -> bool
val is_vernac_proof_ending_command : Vernacexpr.vernac_expr -> bool

(* type hyp = (identifier * constr option * constr) * string *)

type hyp = env * evar_map *
           ((identifier*string) * constr option * constr) * (string * string)
type meta = env * evar_map * string
type concl = env * evar_map * constr * string
type goal = hyp list * concl

val get_current_goals : unit -> goal list

val get_current_pm_goal : unit -> goal

type context_menu = (string * string) list

val goals : unit -> ((string * context_menu) list * string * context_menu) list
val no_goal_message : unit -> string

val process_exn : exn -> string*(Util.loc option)

val is_in_coq_lib : string -> bool
val is_in_coq_path : string -> bool
val is_in_loadpath : string -> bool

val make_cases : string -> string list list


type tried_tactic =
  | Interrupted
  | Success of int (* nb of goals after *)
  | Failed

(* Message to display in lower status bar. *)

val current_status : unit -> string
