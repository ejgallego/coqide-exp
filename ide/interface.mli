(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(** * Declarative part of the interface of CoqIde calls to Coq *)

(** * Generic structures *)

type richpp = Richpp.richpp


type hint = (string * string) list
(** A list of tactics applicable and their appearance *)

type option_name = string list

type option_value =
  | BoolValue      of bool
  | IntValue       of int option
  | StringValue    of string
  | StringOptValue of string option

(** Summary of an option status *)
type option_state = {
  opt_sync  : bool;
  (** Whether an option is synchronous *)
  opt_depr  : bool;
  (** Wheter an option is deprecated *)
  opt_name  : string;
  (** A short string that is displayed when using [Test] *)
  opt_value : option_value;
  (** The current value of the option *)
}


(** Calls result *)

type state_id = Feedback.state_id
type edit_id = Feedback.edit_id

