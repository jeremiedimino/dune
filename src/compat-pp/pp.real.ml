let prog_name = Filename.basename Sys.executable_name

let dump_ast = ref false

module Wrap_lexer = struct
  let save_loc (lexbuf : Lexing.lexbuf) =
    (lexbuf.lex_start_p,
     lexbuf.lex_curr_p)

  let restore_loc (lexbuf : Lexing.lexbuf) (start, curr) =
    lexbuf.lex_start_p <- start;
    lexbuf.lex_curr_p <- curr

  let wrap (lexer : Lexing.lexbuf -> Parser.token) =
    let lexbuf = ref None in
    let stream =
      Stream.from (fun _ ->
        match !lexbuf with
        | None -> assert false
        | Some lexbuf ->
          let tok = lexer lexbuf in
          let loc = save_loc lexbuf in
          Some (tok, loc))
    in
    let pending = Queue.create () in
    let add x = Queue.push x pending in
    let junk n =
      for i = 1 to n do
        Stream.junk stream
      done
    in
    let feed () =
      match Stream.npeek stream 3 with
      | (LPAREN, loc1) :: (LET, loc2) :: (PLUS, loc3) :: _ ->
        junk 3;
        add (LPAREN, loc1);
        add (LIDENT "let__plus", (fst loc2, snd loc3))
      | (LPAREN, loc1) :: (AND, loc2) :: (PLUS, loc3) :: _ ->
        junk 3;
        add (LPAREN, loc1);
        add (LIDENT "and__plus", (fst loc2, snd loc3))
      | (LET, loc1) :: (PLUS, loc2) :: _ ->
        junk 2;
        add (LET, loc1);
        add (LBRACKETAT, loc2);
        add (LIDENT "+", loc2);
        add (RBRACKET, loc2)
      | (AND, loc1) :: (PLUS, loc2) :: _ ->
        junk 2;
        add (AND, loc1);
        add (LBRACKETAT, loc2);
        add (LIDENT "+", loc2);
        add (RBRACKET, loc2)
    in
    let lexer lb =
      (match !lexbuf with
       | None -> lexbuf := Some lb
       | Some x -> assert (x == lb));
      if Queue.is_empty pending then feed ();
      let tok, loc = Queue.pop pending in
      restore_loc lb loc;
      tok
    in
    lexbuf
end

module Map_ast = struct

end

let process_file fn ~magic ~parse ~print =
  let lexbuf = Lexing.from_channel (open_in_bin fn) in
  Location.init lexbuf fn;
  Location.input_lexbuf := Some lexbuf;
  let lexer = Wrap_lexer.wrap Lexer.token in
  let ast = parse lexer lexbuf in
  if !dump_ast then begin
    output_string stdout magic;
    output_value stdout fn;
    output_value stdout ast
  end else
    Format.printf "%a@?" print ast

let process_file fn =
  match Filename.extension fn with
  | ".ml" ->
    process_file
      ~magic:Config.ast_impl_magic_number
      ~parse:Parse.implementation
      ~print:Pprint.implementation
  | ".mli" ->
    process_file
      ~magic:Config.ast_intf_magic_number
      ~parse:Parse.interface
      ~print:Pprint.interface
  | _ ->
    Printf.eprintf "%s: Don't know what to do with %s.\n%!"
      prog_name fn;
    exit 2

let () =
  let args =
    Arg.align
      [ "-dump-ast", Arg.Set dump_ast,
        " Output a binary AST rather than a pretty-printed source file"
      ]
  in
  let usage = Printf.sprintf "Usage: %s [-dump-ast] FILES" in
  Arg.parse args process_file usage
