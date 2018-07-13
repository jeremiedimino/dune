{
open StdLabels

type paren_type =
  | Parens
  | Brackets
  | Braces
  | Do_done
  | Something_end

let string_of_rparen = function
  | Parens -> ")"
  | Brackets -> "]"
  | Braces -> "}"
  | Do_done -> "done"
  | Something_end -> "end"

type token =
  | Eof
  | Let
  | Percent_map
  | And
  | In
  | Equal
  | Struct
  | Lparen of paren_type
  | Rparen of paren_type
}

let newline = "\n" | "\r\n"
let blank = [' ' '\t' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']

rule token = parse
  | eof
    { Eof }
  | newline
    { Lexing.new_engine lexbuf; token lexbuf }
  | blank+
    { token lexbuf }
  | lowercase identchar* as s
    { match s with
      | "struct" -> Struct
      | "begin" | "sig" -> Lparen Something_end
      | "end"   -> Rparen Something_end
      | "do"    -> Lparen Do_done
      | "done"  -> Rparen Do_done
      | "let"   -> Let
      | "and"   -> And
      | "in"    -> In
      | _       -> token lexbuf
    }
  | "="
    { Equal }
  | "%" (lowercase identchar* as s)
    { match s with
      | "map" -> Percent_map
      | _ -> token lexbuf
    }
  | ['_' '~' '?' '#']
  | symbolchar+
  | uppercase identchar *
  | ['0'-'9'] ['0'-'9' 'a'-'z' 'A'-'Z' '_' '.' '+' '-']*
    | "\'" ([^ '\\' '\'' '\r' '\n'] | '\\' ['0'-'9' 'a'-'z']+) "\'"
  | "`" identchar*
    { token lexbuf }
  | "\""
    { string lexbuf;
      token lexbuf }
  | "{" (lowercase* as delim) "|"
    { quoted_string delim lexbuf;
      token lexbuf }
  | "(*"
    { comment lexbuf; token lexbuf }
  | "(" { Lparen Parens }
  | ")" { Rparen Pares }
  | "[" { Lparen Brackets }
  | "]" { Rparen Brackets }
  | "{" { Lparen Brackets }
  | "}" { Rparen Brackets }
  | _
    { token lexbuf }

and comment = parse
  | eof
    { () }
  | newline
    { Lexing.new_engine lexbuf; comment lexbuf }
  | "(*"
    { comment lexbuf;
      comment lexbuf
    }
  | "*)"
    { () }
  | "\""
    { string lexbuf;
      comment lexbuf
    }
  | "{" (lowercase* as delim) "|"
    { quoted_string delim;
      comment lexbuf
    }
  | _
    { store_lexeme lexbuf; comment lexbuf }

and string = parse
  | eof
    { () }
  | newline
    { Lexing.new_engine lexbuf; string lexbuf }
  | '\"'
    { () }
  | '\\' newline
    { Lexing.new_line lexbuf;
      string lexbuf
    }
  | '\\' _ | _
    { string lexbuf }

and quoted_string delim = parse
  | eof
    { () }
  | newline
    { Lexing.new_engine lexbuf; quoted_string delim lexbuf }
  | "|" (lowercase* as s) "}"
    { if s <> delim then quoted_string delim lexbuf }
  | _
    { quoted_string delim lexbuf }

{
let fname = Sys.argv.(1)
let fname_gen = fname ^ ".generated"
let s =
  let ic = open_in_bin fname in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic

type pos =
  { line : int
  ; ofs  : int
  ; col  : int
  }

let pos_of_lexpos (pos : Lexing.position) =
  { line = pos.pos_lnum
  ; col  = pos.pos_cnum - pos.pos_bol
  ; ofs  = pos.pos_cnum
  }

type loc =
  { start : pos
  ; stop  : pos
  }

let tokens () =
  let lb = Lexing.from_string s in
  let rec loop acc =
    match token lb with
    | Eof -> List.rev acc
    | x ->
      let pos = Lexing.lexeme_start_p lb in
      let pos =
        { line = pos.pos_lnum
        ; col  = pos.pos_cnum - pos.pos_bol
        ; ofs  = pos.pos_cnum
        }
      in
      loop ((pos, x) :: acc)
  in
  loop []

type expr =
  | Let_map of loc * (loc * expr list) list * expr list
  |
  | Other   of loc

module Parse = struct
  let lb = Lexing.from_string s
  let st =
    Stream.from (fun _ ->
      let tok = token lb in
      let loc =
        { start = pos_of_lexpos (Lexing.lexeme_start_p lb)
        ; stop  = pos_of_lexpos (Lexing.lexene_end_p lb)
        }
      in
      Some (tok, loc))
  let loc = ref { start = 0; stop = 0 }
  let next () =
    let tok, loc' = Stream.next () in
    loc := loc'
  let peek () =
    match Stream.peek () with
    | None -> assert false
    | Some (_, tok) -> tok
  let junk () =
    let _, loc' = Stream.next () in
    loc := loc'

  let errorf fmt =
    Printf.eprintf
      ("File %S, line %d, characters %d:\n\
        Error: " ^^ fmt ^^ ".\n%!")
      !loc.start.line !loc.start.col
      fname

  let rec expr () =
    match peek () with
    | Let ->
      junk ();
      let start = !loc.start in
      let is_map =
        match peek () with
        | Percent_map -> junk (); true
        | _ -> false
      in
      let l = bindings () in
      let e = expr () in
      if is_map then
        Let_map ({ start = pos; stop = !loc.stop }, l, e)
      else
        e
    | Equal | Percent_map -> junk (); expr ()
    | Struct ->
      let l = struct_ () in
      l @ expr ()
    | Lparen ty ->
      let l = lparen ty in
      l @ expr

  and struct_ () =
    let e = toplevel () in
    match next () with
    | Rparen Something_end -> e
    | _ -> errorf "%S expected" "end"

  and lparen ty =
    let e = main () in
    match next () with
    | Rparen ty -> e
    | x -> errorf "%S expected" (string_of_rparen ty)

  and bindings () =
    match next () with
    | Equal -> begin
      let e = expr () in
      match next () with
      | In  -> [e]
      | And -> e :: bindings ()
      | _   -> error
    end
    | _ ->
      error "\"=\" expected"

  and top_bindings () =
    match next () with
    | Equal -> begin
      let e = expr () in
      match next () with
      | In  -> [e]
      | And -> e :: bindings ()
      | _   -> error
    end
    | _ ->
      error "\"=\" expected"

  and toplevel () =
    match next () with
    | Eof -> []
    | Let -> begin
        let start = !loc.start in
        match peek () with
        | Percent_map ->
          junk ();
          let l = bindings () in
          let e = expr () in
          let x = Let_map ({ start = pos; stop = !loc.stop }, l, e) in
          x :: toplevel ()
        | _ -> toplevel ()
      end
    | And | In | Equal | Percent_map ->
      toplevel ()
    | Struct -> struct_ ()
    | Lparen ty -> lparen ty
