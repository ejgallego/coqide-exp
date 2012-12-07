(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

{
  exception Unterminated
}

let space =
  [' ' '\n' '\r' '\t' '\012'] (* '\012' is form-feed *)

let undotted_sep = [ '{' '}' '-' '+' '*' ]

let dot_sep = '.' (space | eof)

rule coq_string = parse
  | "\"\"" { coq_string lexbuf }
  | "\"" { () }
  | eof { () }
  | _ { coq_string lexbuf }

and comment = parse
  | "(*" { let _  = comment lexbuf in comment lexbuf }
  | "\"" { let () = coq_string lexbuf in comment lexbuf }
  | "*)" { Some (Lexing.lexeme_start lexbuf + 1) }
  | eof { None }
  | _ { comment lexbuf }

(** NB : [mkiter] should be called on increasing offsets *)

and sentence initial stamp = parse
  | "(*" {
      match comment lexbuf with
        | None -> raise Unterminated
        | Some comm_last ->
          (* A comment alone is a sentence.
	     A comment in a sentence doesn't terminate the sentence.
             Note: comm_end is the position of the comment final ')' *)
          if initial then stamp comm_last Tags.Script.comment_sentence;
          sentence initial stamp lexbuf
    }
  | "\"" {
      let () = coq_string lexbuf in
      sentence false stamp lexbuf
    }
  | ".." {
      (* We must have a particular rule for parsing "..", where no dot
	 is a terminator, even if we have a blank afterwards
	 (cf. for instance the syntax for recursive notation).
	 This rule and the following one also allow to treat the "..."
	 special case, where the third dot is a terminator. *)
      sentence false stamp lexbuf
    }
  | dot_sep {
      (* The usual "." terminator *)
      stamp (Lexing.lexeme_start lexbuf) Tags.Script.sentence;
      sentence true stamp lexbuf
    }
  | undotted_sep {
      (* Separators like { or } and bullets * - + are only active
	 at the start of a sentence *)
      if initial then stamp (Lexing.lexeme_start lexbuf) Tags.Script.sentence;
      sentence initial stamp lexbuf
    }
  | space+ {
       (* Parsing spaces is the only situation preserving initiality *)
       sentence initial stamp lexbuf
    }
  | _ {
      (* Any other characters *)
      sentence false stamp lexbuf
    }
  | eof { if initial then () else raise Unterminated }

{

  (** Parse sentences in string [slice], tagging last characters
      of sentences with the [stamp] function.
      It will raise [Unterminated] if [slice] ends with an unfinished
      sentence.
  *)

  let delimit_sentences stamp slice =
    sentence true stamp (Lexing.from_string slice)

}
