(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

class type detachable_signals =
  object
    inherit GContainer.container_signals
    method attached : callback:(GObj.widget -> unit) -> unit
    method detached : callback:(GObj.widget -> unit) -> unit
  end

class detachable : ([> Gtk.box] as 'a) Gobject.obj ->
  object
    inherit GPack.box_skel
    val obj : Gtk.box Gobject.obj
    method connect : detachable_signals
    method child : GObj.widget
    method show : unit
    method hide : unit
    method visible : bool
    method title : string
    method set_title : string -> unit
    method button : GButton.button

  end

val detachable :
  ?title:string ->
  ?homogeneous:bool ->
  ?spacing:int ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) -> ?show:bool -> unit -> detachable


