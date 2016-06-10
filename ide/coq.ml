(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Ideutils

(** The plan here is to associate the monad to a SerAPI request  *)

(** Useful stuff *)

let interrupter = ref (fun pid -> Unix.kill pid Sys.sigint)

(** * The structure describing a coqtop sub-process *)

let gio_channel_of_descr_socket = ref Glib.Io.channel_of_descr

module GlibMainLoop = struct
  type async_chan = Glib.Io.channel
  type watch_id = Glib.Io.id
  type condition = Glib.Io.condition
  let add_watch ~callback chan =
    Glib.Io.add_watch ~cond:[`ERR; `HUP; `IN; `NVAL; `PRI] ~callback chan
  let remove_watch x = try Glib.Io.remove x with Glib.GError _ -> ()
  let read_all = Ideutils.io_read_all
  let async_chan_of_file fd = Glib.Io.channel_of_descr fd
  let async_chan_of_socket s = !gio_channel_of_descr_socket s
end

module CoqTop = Spawn.Async(GlibMainLoop)

type coq_handle = CoqTop.process

(** Coqtop process status :
  - New    : a process has been spawned, but not initialized via [init_coqtop].
             It will reject tasks given via [try_grab].
  - Ready  : no current task, accepts new tasks via [try_grab].
  - Busy   : has accepted a task via [init_coqtop] or [try_grab],
             It will reject other tasks for the moment
  - Closed : the coqide buffer has been closed, we discard any further task.
*)

type coq_status = New | Ready | Busy | Closed

(** An abstract copy of unit.a *)

type void = Void

type 'a task = coq_handle -> ('a -> void) -> void

type coqtop = {
  (* actual coqtop process and its status *)
  coq_args         : string array;
  mutable handle   : coq_handle;
  mutable status   : coq_status;
}

type coqtop_opts = {
  argv       : string array;              (* Command line arguments *)
  (* fb_handler : Feedback.feedback -> unit; (\* Feedback handler       *\) *)
}

let return (x : 'a) : 'a task =
  (); fun _ k -> k x

let bind (m : 'a task) (f : 'a -> 'b task) : 'b task =
  (); fun h k -> m h (fun x -> f x h k)

let seq (m : unit task) (n : 'a task) : 'a task =
  (); fun h k -> m h (fun () -> n h k)

let lift (f : unit -> 'a) : 'a task =
  (); fun _ k -> k (f ())

let print_exception = function
  | e -> Printexc.to_string e

(** Main processing function, it should write to the structure!  *)
let coqtop_callback : CoqTop.callback = fun clist ~read_all -> true

(** This launches a fresh handle from its command line arguments. *)
let start_coqtop ?env args =
  let prog      = ""                                            in
  let proc, _oc = CoqTop.spawn ?env prog args (coqtop_callback) in
  proc

let mkready coqtop =
  fun () -> coqtop.status <- Ready; Void

let spawn_coqtop opts =
  {
    coq_args         = opts.argv;
    handle           = start_coqtop opts.argv;
    status           = New;
  }

let close_coqtop coqtop =
  coqtop.status <- Closed;
  CoqTop.kill coqtop.handle;
  ignore(CoqTop.wait coqtop.handle)

let reset_coqtop coqtop =
  close_coqtop coqtop;
  coqtop.handle <- start_coqtop coqtop.coq_args;
  coqtop.status <- New

let set_feedback_handler _ _ = ()

let is_computing  coqtop = (coqtop.status = Busy)
let get_arguments coqtop = coqtop.coq_args

(* For closing a coqtop, we don't try to send it a Quit call anymore,
   but rather close its channels:
    - a listening coqtop will handle this just as a Quit call
    - a busy coqtop will anyway have to be killed *)


let process_task coqtop task =
  assert (coqtop.status = Ready || coqtop.status = New);
  coqtop.status <- Busy;
  ignore (task coqtop.handle (mkready coqtop))

let try_grab coqtop task abort =
  match coqtop.status with
    | Closed -> ()
    | Busy | New -> abort ()
    | Ready -> process_task coqtop task

let init_coqtop coqtop task =
  assert (coqtop.status = New);
  process_task coqtop task

(** * Calls to coqtop *)

(** Cf [Ide_intf] for more details *)

type location = (int * int) option (* start and end of the error *)

type 'a value =
  | Good of 'a
  | Fail of (Stateid.t * location * Richpp.richpp)

type 'a query = 'a value task

let proto_call ?(logger=default_logger) call handle k =
  (** Send messages to coqtop and prepare the decoding of the answer *)
  Minilib.log ("Start proto_call ");
  (* XXX Do the call *)
  (* handle.waiting_for <- Some (mk_ccb *)
  Minilib.log "End proto_call";
  Void

(**************************************************************************)
(* Stop_worker                                                            *)
(**************************************************************************)
let stop_worker x =
  proto_call (Sertop_protocol.Quit)

let break_coqtop coqtop workers =
  if coqtop.status = Busy then
    try !interrupter (CoqTop.unixpid coqtop.handle)
    with _ -> Minilib.log "Error while sending Ctrl-C"
  else
    let rec aux = function
    | [] -> Void
    | w :: ws -> stop_worker w coqtop.handle (fun _ -> aux ws)
    in
      let Void = aux workers in ()

module PrintOpt =
struct
  type t = string list

  (* Boolean options *)

  let implicit = ["Printing"; "Implicit"]
  let coercions = ["Printing"; "Coercions"]
  let raw_matching = ["Printing"; "Matching"]
  let notations = ["Printing"; "Notations"]
  let all_basic = ["Printing"; "All"]
  let existential = ["Printing"; "Existential"; "Instances"]
  let universes = ["Printing"; "Universes"]

  type bool_descr = { opts : t list; init : bool; label : string }

  let bool_items = [
    { opts = [implicit]; init = false; label = "Display _implicit arguments" };
    { opts = [coercions]; init = false; label = "Display _coercions" };
    { opts = [raw_matching]; init = true;
      label = "Display raw _matching expressions" };
    { opts = [notations]; init = true; label = "Display _notations" };
    { opts = [all_basic]; init = false;
      label = "Display _all basic low-level contents" };
    { opts = [existential]; init = false;
      label = "Display _existential variable instances" };
    { opts = [universes]; init = false; label = "Display _universe levels" };
    { opts = [all_basic;existential;universes]; init = false;
      label = "Display all _low-level contents" }
  ]

  (** The current status of the boolean options *)

  let current_state = Hashtbl.create 11

  let set opt v = Hashtbl.replace current_state opt v

  let reset () =
    let init_descr d = List.iter (fun o -> set o d.init) d.opts in
    List.iter init_descr bool_items

  let _ = reset ()

  (** Integer option *)

  let width = ["Printing"; "Width"]
  let width_state = ref None
  let set_printing_width w = width_state := Some w

  (** Transmitting options to coqtop *)

  let enforce h k =
    let mkopt o v acc = (o, Interface.BoolValue v) :: acc in
    let opts = Hashtbl.fold mkopt current_state []        in
    let _opts = (width, Interface.IntValue !width_state) :: opts in
    (* XXX *)
    proto_call () h
      (function
	| Good () -> k ()
	| _       -> failwith "Cannot set options. Resetting coqtop")

end

type verbose = bool

(**************************************************************************)
(* Add                                                                    *)
(**************************************************************************)

let add ?(logger=default_logger)   x = proto_call ~logger (Sertop_protocol.Quit)

(**************************************************************************)
(* Edit_at                                                                *)
(**************************************************************************)

let edit_at                        i = proto_call         (Sertop_protocol.Quit)

(**************************************************************************)
(* Query                                                                  *)
(**************************************************************************)
let query ?(logger=default_logger) x = proto_call ~logger (Sertop_protocol.Quit)

(**************************************************************************)
(* mkcases                                                                *)
(**************************************************************************)
let mkcases                        s = proto_call         (Sertop_protocol.Quit)

(**************************************************************************)
(* Status                                                                 *)
(**************************************************************************)
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
let status ?logger force             = proto_call ?logger (Sertop_protocol.Quit)

(**************************************************************************)
(* Init                                                                   *)
(**************************************************************************)
let init x                           = proto_call         (Sertop_protocol.Quit)

(**************************************************************************)
(* Goals                                                                  *)
(**************************************************************************)

(** Fetching the list of current goals. Return [None] if no proof is in
    progress, [Some gl] otherwise. *)
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

let goals ?logger x h k =
  PrintOpt.enforce h (fun () -> proto_call ?logger (Sertop_protocol.Quit) h k)

(**************************************************************************)
(* Evars                                                                  *)
(**************************************************************************)

(** Retrieve the list of unintantiated evars in the current proof. [None] if no
    proof is in progress. *)
type evar = {
  evar_info : string;
  (** A string describing an evar: type, number, environment *)
}

let evars x h k =
  PrintOpt.enforce h (fun () -> proto_call (Sertop_protocol.Quit) h k)

(**************************************************************************)
(* Hints                                                                  *)
(**************************************************************************)
type hint = (string * string) list
let hints x                          = proto_call         (Sertop_protocol.Quit)

(**************************************************************************)
(* Search                                                                 *)
(**************************************************************************)

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
let search flags                     = proto_call         (Sertop_protocol.Quit)

let version () = "Implement as a protocol call"
