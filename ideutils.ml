
open Preferences

exception Forbidden


let debug = Options.debug

let prerr_endline s =
  if !debug then (prerr_endline s;flush stderr)
let prerr_string s =
  if !debug then (prerr_string s;flush stderr)

let lib_ide = Filename.concat Coq_config.coqlib "ide"
  
let get_insert input_buffer = input_buffer#get_iter_at_mark `INSERT

let is_char_start c = let code = Char.code c in code < 0x80 || code >= 0xc0

let byte_offset_to_char_offset s byte_offset = 
  if (byte_offset < String.length s) then begin
    let count_delta = ref 0 in
    for i = 0 to byte_offset do
      let code = Char.code s.[i] in
      if code >= 0x80 && code < 0xc0 then incr count_delta
    done;
    byte_offset - !count_delta
  end
  else begin
    let count_delta = ref 0 in
    for i = 0 to String.length s - 1 do
      let code = Char.code s.[i] in
      if code >= 0x80 && code < 0xc0 then incr count_delta
    done;
    byte_offset - !count_delta
  end

let process_pending () =
  prerr_endline "Pending process";()
(*  try 
    while Glib.Main.pending () do 
      ignore (Glib.Main.iteration false)
    done
  with e -> 
    prerr_endline "Pending problems : expect a crash very soon";
    raise e
*)

let print_id id =
  prerr_endline ("GOT sig id :"^(string_of_int (Obj.magic id)))

let try_convert s = 
  try
    if Glib.Utf8.validate s then s else
      (prerr_endline 
	 "Coqide warning: input is not UTF-8 encoded. Trying to convert from locale.";
       Glib.Convert.locale_to_utf8 s)
  with _ -> 
    "(* Fatal error: wrong encoding in input.
Please set your locale according to your file encoding.*)"

let do_convert s = 
  if Glib.Utf8.validate s then s else
    try 
      (prerr_endline 
	 "Coqide warning: input is not UTF-8 encoded. Trying to convert from locale.";
       Glib.Convert.locale_to_utf8 s)
    with _ -> 
      (prerr_endline 
	 "Coqide warning: input is not even LOCALE encoded. Trying to convert from fr_FR.";
       Glib.Convert.convert s ~to_codeset:"UTF-8" ~from_codeset:"ISO-8859-1")

let try_export file_name s = 
  try 
    let s = 
      if (fst (Glib.Convert.get_charset ())) then
	s 
      else
	(try Glib.Convert.locale_from_utf8 s 
	 with _ -> 
	   try 
	     prerr_endline "Warning: exporting to ISO8859-1";
	     Glib.Convert.convert s ~to_codeset:"UTF-8" ~from_codeset:"ISO-8859-1"
	   with _ -> 
	     prerr_endline "Warning: exporting to utf8";s)
    in
    let oc = open_out file_name in
    output_string oc s;
    close_out oc;
    true
  with e -> prerr_endline (Printexc.to_string e);false

let browse url =
  let l,r = !current.cmd_browse in
  ignore (Sys.command (l ^ url ^ r))

let url_for_keyword =
  let ht = Hashtbl.create 97 in
  begin try
    let cin = open_in (Filename.concat lib_ide "index_urls.txt") in
    try while true do
      let s = input_line cin in
      try 
	let i = String.index s ',' in
	let k = String.sub s 0 i in
	let u = String.sub s (i + 1) (String.length s - i - 1) in
	Hashtbl.add ht k u
      with _ ->
	()
    done with End_of_file ->
      close_in cin
  with _ ->
    ()
  end;
  (Hashtbl.find ht : string -> string)


let browse_keyword text = 
  try 
    let u = url_for_keyword text in browse (!current.doc_url ^ u) 
  with _ -> ()

let my_stat f = try Some (Unix.stat f) with _ -> None

let revert_timer = ref None
let disconnect_revert_timer () = match !revert_timer with
  | None -> ()
  | Some id -> GMain.Timeout.remove id; revert_timer := None

let highlight_timer = ref None
let set_highlight_timer f = 
  match !highlight_timer with 
    | None -> 
	revert_timer := 
      Some (GMain.Timeout.add ~ms:2000 
	      ~callback:(fun () -> f (); highlight_timer := None; true))
    | Some id -> 
	GMain.Timeout.remove id;
	revert_timer := 
	Some (GMain.Timeout.add ~ms:2000 
		~callback:(fun () -> f (); highlight_timer := None; true))


(* Get back the standard coq out channels *)
let read_stdout,clear_stdout =
  let out_buff = Buffer.create 100 in
  Pp_control.std_ft := Format.formatter_of_buffer out_buff;
  (fun () -> Format.pp_print_flush !Pp_control.std_ft (); 
     let r = Buffer.contents out_buff in
     Buffer.clear out_buff; r),
  (fun () -> 
     Format.pp_print_flush !Pp_control.std_ft (); Buffer.clear out_buff)


let last_dir = ref ""
let select_file ~title ?(dir = last_dir) ?(filename="") () =
  let fs =
    if Filename.is_relative filename then begin
      if !dir <> "" then
        let filename = Filename.concat !dir filename in 
        GWindow.file_selection ~modal:true ~title ~filename ()
      else
        GWindow.file_selection ~modal:true ~title ()
    end else begin
      dir := Filename.dirname filename;
      GWindow.file_selection ~modal:true ~title ~filename ()
    end
  in
  fs#complete ~filter:"";
  ignore (fs#connect#destroy ~callback: GMain.Main.quit);
  let file = ref None in 
  ignore (fs#ok_button#connect#clicked ~callback:
    begin fun () ->
      file := Some fs#get_filename; 
      dir := Filename.dirname fs#get_filename;
      fs#destroy ()
    end);
  ignore (fs # cancel_button # connect#clicked ~callback:fs#destroy);
  fs # show ();
  GMain.Main.main ();
  !file


let find_tag_start (tag :GText.tag) (it:GText.iter) =
  let it = it#copy in
  let tag = Some tag in
  while not (it#begins_tag tag) && it#nocopy#backward_char do
    ()
  done;
  it
let find_tag_stop (tag :GText.tag) (it:GText.iter) =
  let it = it#copy in
  let tag = Some tag in
  while not (it#ends_tag tag) && it#nocopy#forward_char do
    ()
  done;
  it
let find_tag_limits (tag :GText.tag) (it:GText.iter) = 
 (find_tag_start tag it , find_tag_stop tag it)

let async = 
  if Sys.os_type <> "Unix" then GtkThread.async else (fun x -> x)
