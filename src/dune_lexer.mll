{
open! Stdune
type first_line =
  { lang    : Loc.t * string
  ; version : Loc.t * string
  }

let make_loc lexbuf : Loc.t =
  { start = Lexing.lexeme_start_p lexbuf
  ; stop  = Lexing.lexeme_end_p   lexbuf
  }

let invalid_lang_line start lexbuf =
  lexbuf.Lexing.lex_start_p <- start;
  User_error.raise ~loc:(Loc.of_lexbuf lexbuf)
    [ Pp.text "Invalid first line, expected: (lang <lang> <version>)" ]
}

let newline   = '\r'? '\n'
let blank     = [' ' '\t']
let atom_char = [^';' '(' ')' '"' '#' '|' '\000'-'\032']
let number    = '0'-'9'+

rule is_script = parse
  | "(* -*- tuareg -*- *)" { true }
  | ""                     { false }

and maybe_first_line = parse
  | '(' blank* "lang"
    { let start = Lexing.lexeme_start_p lexbuf in
      let lang    = atom start lexbuf in
      let version = atom start lexbuf in
      first_line_rparen_end start lexbuf;
      Some { lang; version }
    }
  | ""
    { None
    }

and atom start = parse
  | blank+
    { atom start lexbuf
    }
  | atom_char+ as s
    { (make_loc lexbuf, s)
    }
  | _ | eof
    { to_eol lexbuf;
      invalid_lang_line start lexbuf
    }

and first_line_rparen_end start = parse
  | blank* ')' blank* (newline | eof as s)
    { if s <> "" then Lexing.new_line lexbuf
    }
  | ""
    { to_eol lexbuf;
      invalid_lang_line start lexbuf
    }

and to_eol = parse
  | [^'\r' '\n']*
    { ()
    }

and eof_reached = parse
  | eof { true  }
  | ""  { false }

and ocaml_location = parse
  | "File \"" ([^ '"']* as fname) "\""
      { ocaml_location2 lexbuf }

  | "" { None }

and ocaml_location2 fname = parse
  | ", line " (number as line)
      { ocaml_location3 fname line line lexbuf }
  | ", lines " (number as line1) (number as line2)
      { ocaml_location3 fname line1 line2 lexbuf }
  | ""
      { None }

and ocaml_location3 fname line1 line2 = parse
  | ", characters " (number as c1) "-" (number as c2)
      { ocaml_location4 fname line1 line2 c1 c2 lexbuf }
  | ""
      { ocaml_location4 fname line1 line2 0 0 lexbuf }

and ocaml_location4 fname line1 line2 c1 c2 = parse
  | ":" eof
      { Some
          { Loc.
            start =
              { Lexing.
                pos_fname = fname
              ; pos_lnum = line1
              ; pos_bol = 0
              ; pos_cnum = c1
              }
          ; stop =
              { Lexing.
                pos_fname = fname
              ; pos_lnum = line2
              ; pos_bol = 0
              ; pos_cnum = c2
              }
          }
      }
  | ""
    { None }

{
  let first_line lb =
    match maybe_first_line lb with
    | Some x -> x
    | None ->
      let start = Lexing.lexeme_start_p lb in
      to_eol lb;
      invalid_lang_line start lb
}
