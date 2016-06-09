(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(** Coq : Interaction with the Coq toplevel *)

(** {5 General structures} *)

type coqtop
(** The structure describing a coqtop sub-process .

    Liveness management of coqtop is automatic. Whenever a coqtop dies abruptly,
    this module is responsible for relaunching the whole process. The reset
    handler set through [set_reset_handler] will be called after such an
    abrupt failure. It is also called when explicitly requesting coqtop to
    reset. *)

type 'a task
(** Coqtop tasks.

    A task is a group of sequential calls to be performed on a coqtop process,
    that ultimately return some content.

    If a task is already sent to coqtop, it is considered busy
    ([is_computing] will answer [true]), and any other task submission
    will be rejected by [try_grab].

    Any exception occurring within the task will trigger a coqtop reset.

    Beware, because of the GTK scheduler, you never know when a task will
    actually be executed. If you need to sequentialize imperative actions, you
    should do so using the monadic primitives.
*)

(* The fail case carries the current state_id of the prover, the GUI
   should probably retract to that point *)
val return : 'a -> 'a task
(** Monadic return of values as tasks. *)

val bind : 'a task -> ('a -> 'b task) -> 'b task
(** Monadic binding of tasks *)

val lift : (unit -> 'a) -> 'a task
(** Return the imperative computation waiting to be processed. *)

val seq : unit task -> 'a task -> 'a task
(** Sequential composition *)

(** {5 Coqtop process management} *)

type reset_kind = Planned | Unexpected
(** A reset may occur accidentally or voluntarily, so we discriminate between
    these. *)

val is_computing : coqtop -> bool
(** Check if coqtop is computing, i.e. already has a current task *)

val spawn_coqtop : string list -> coqtop
(** Create a coqtop process with some command-line arguments. *)

val set_reset_handler : coqtop -> (reset_kind -> unit task) -> unit
(** Register a handler called when a coqtop dies (badly or on purpose) *)

val set_feedback_handler : coqtop -> (Feedback.feedback -> unit) -> unit
(** Register a handler called when coqtop sends a feedback message *)

val init_coqtop : coqtop -> unit task -> unit
(** Finish initializing a freshly spawned coqtop, by running a first task on it.
    The task should run its inner continuation at the end. *)

val break_coqtop : coqtop -> string list -> unit
(** Interrupt the current computation of coqtop or the worker if coqtop it not running. *)

val close_coqtop : coqtop -> unit
(** Close coqtop. Subsequent requests will be discarded. Hook ignored. *)

val reset_coqtop : coqtop -> unit
(** Reset coqtop. Pending requests will be discarded. The reset handler
    of coqtop will be called with [Planned] as first argument *)

val get_arguments : coqtop -> string list
(** Get the current arguments used by coqtop. *)

val set_arguments : coqtop -> string list -> unit
(** Set process arguments. This also forces a planned reset. *)

(** In win32, sockets are not like regular files *)
val gio_channel_of_descr_socket : (Unix.file_descr -> Glib.Io.channel) ref

(** {5 Task processing} *)

val try_grab : coqtop -> unit task -> (unit -> unit) -> unit
(** Try to schedule a task on a coqtop. If coqtop is available, the task
    callback is run (asynchronously), otherwise the [(unit->unit)] callback
    is triggered.
    - If coqtop ever dies during the computation, this function restarts coqtop
      and calls the restart hook with the fresh coqtop.
    - If the argument function raises an exception, a coqtop reset occurs.
    - The task may be discarded if a [close_coqtop] or [reset_coqtop] occurs
      before its completion.
    - The task callback should run its inner continuation at the end. *)

(** {5 Atomic calls to coqtop} *)

(** These atomic calls can be combined to form arbitrary multi-call
    tasks.  They correspond to the protocol calls. Note that each call
    is asynchronous: it will return immediately, but the inner
    callback will be executed later to handle the call answer when
    this answer is available.  Except for interp, we use the default
    logger for any call. *)

type location = (int * int) option (* start and end of the error *)

type 'a value =
  | Good of 'a
  | Fail of (Stateid.t * location * Richpp.richpp)

type 'a query = 'a value task

(** A type abbreviation for coqtop specific answers *)

type verbose = bool
(**  [add ((s,eid),(sid,v))] adds the phrase [s] with edit id [eid]
     on top of the current edit position (that is asserted to be [sid])
     verbosely if [v] is true.  The response [(id,(rc,s)] is the new state
     [id] assigned to the phrase, some output [s].  [rc] is [Inl] if the new
     state id is the tip of the edit point, or [Inr tip] if the new phrase
     closes a focus and [tip] is the new edit tip *)
val add :
     ?logger:Ideutils.logger
  -> (string * Feedback.edit_id) * (Stateid.t * verbose)
  -> (Stateid.t * ((unit, Stateid.t) Util.union * string)) query

(** [edit_at id] declares the user wants to edit just after [id].
    The response is [Inl] if the document has been rewound to that point,
    [Inr (start,(stop,tip))] if [id] is in a zone that can be focused.
    In that case the zone is delimited by [start] and [stop] while [tip]
    is the new document [tip].  Edits made by subsequent [add] are always
    performend on top of [id]. *)
val edit_at :
     Stateid.t
  -> (unit, Stateid.t * (Stateid.t * Stateid.t)) Util.union query

(** [query s id] executes [s] at state [id] and does not record any state
    change but for the printings that are sent in response *)
val query : ?logger:Ideutils.logger -> string * Stateid.t -> string query

(** The status, for instance "Ready in SomeSection, proving Foo", the
    input boolean (if true) forces the evaluation of all unevaluated
    statements *)
type status = {
  status_path : string list;
  (** Module path of the current proof *)
  status_proofname : string option;
  (** Current proof name. [None] if no focussed proof is in progress *)
  status_allproofs : string list;
  (** List of all pending proofs. Order is not significant *)
  status_proofnum : int;
  (** An id describing the state of the current proof. *)
}

val status : ?logger:Ideutils.logger -> bool -> status query

(** The type of coqtop goals *)
type goal = {
  goal_id : string;
  (** Unique goal identifier *)
  goal_hyp : Richpp.richpp list;
  (** List of hypotheses *)
  goal_ccl : Richpp.richpp;
  (** Goal conclusion *)
}

type 'a pre_goals = {
  fg_goals : 'a list;
  (** List of the focussed goals *)
  bg_goals : ('a list * 'a list) list;
  (** Zipper representing the unfocused background goals *)
  shelved_goals : 'a list;
  (** List of the goals on the shelf. *)
  given_up_goals : 'a list;
  (** List of the goals that have been given up *)
}

type goals = goal pre_goals

(** Fetching the list of current goals. Return [None] if no proof is in
    progress, [Some gl] otherwise. *)
val goals : ?logger:Ideutils.logger -> unit -> goals option query

(** Retrieve the list of unintantiated evars in the current proof. [None] if no
    proof is in progress. *)
type evar = {
  evar_info : string;
  (** A string describing an evar: type, number, environment *)
}

val evars      : unit -> evar list option query

(** Retrieving the tactics applicable to the current goal. [None] if there is
    no proof in progress. *)
type hint = (string * string) list
val hints : unit -> (hint list * hint) option query

(** Create a "match" template for a given inductive type.
    For each branch of the match, we list the constructor name
    followed by enough pattern variables. *)
val mkcases    : string -> string list list query

(** Initialize, and return the initial state id.  The argument is the filename.
    If its directory is not in dirpath, it adds it.  It also loads
    compilation hints for the filename. *)
val init       : string option -> Stateid.t query

val stop_worker: string -> unit query

(** A specialized version of [raw_interp] dedicated to set/unset options. *)

module PrintOpt :
sig
  type t (** Representation of an option *)

  type bool_descr = { opts : t list; init : bool; label : string }

  val bool_items : bool_descr list

  val set : t -> bool -> unit
  val set_printing_width : int -> unit

  (** [enforce] transmits to coq the current option values.
      It is also called by [goals] and [evars] above. *)

  val enforce : unit task
end

(** {5 Search} *)

type search_constraint =
(** Whether the name satisfies a regexp (uses Ocaml Str syntax) *)
| Name_Pattern of string
(** Whether the object type satisfies a pattern *)
| Type_Pattern of string
(** Whether some subtype of object type satisfies a pattern *)
| SubType_Pattern of string
(** Whether the object pertains to a module *)
| In_Module of string list
(** Bypass the Search blacklist *)
| Include_Blacklist

(** A list of search constraints; the boolean flag is set to [false] whenever
    the flag should be negated. *)
type search_flags = (search_constraint * bool) list

(** A named object in Coq. [coq_object_qualid] is the shortest path defined for
    the user. [coq_object_prefix] is the missing part to recover the fully
    qualified name, i.e [fully_qualified = coq_object_prefix + coq_object_qualid].
    [coq_object_object] is the actual content of the object. *)
type 'a coq_object = {
  coq_object_prefix : string list;
  coq_object_qualid : string list;
  coq_object_object : 'a;
}

(** Search for objects satisfying the given search flags. *)
val search     : search_flags -> string coq_object list query

(** {5 Miscellaneous} *)

val short_version : unit -> string
(** Return a short phrase identifying coqtop version and date of compilation, as
    given by the [configure] script. *)

val version : unit -> string
(** More verbose description, including details about libraries and
    architecture. *)

val filter_coq_opts : string list -> string list
(** * Launch a test coqtop processes, ask for a correct coqtop if it fails.
    @return the list of arguments that coqtop did not understand
    (the files probably ..). This command may terminate coqide in
    case of trouble.  *)

val check_connection : string list -> unit
(** Launch a coqtop with the user args in order to be sure that it works,
    checking in particular that Prelude.vo is found. This command
    may terminate coqide in case of trouble *)

val interrupter : (int -> unit) ref

