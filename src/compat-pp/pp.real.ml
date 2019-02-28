open ListLabels

let prog_name = Filename.basename Sys.executable_name

let dump_ast = ref false

module Wrap_lexer = struct
  let save_loc (lexbuf : Lexing.lexbuf) =
    (lexbuf.lex_start_p,
     lexbuf.lex_curr_p)

  let restore_loc (lexbuf : Lexing.lexbuf) (start, curr) =
    lexbuf.lex_start_p <- start;
    lexbuf.lex_curr_p <- curr

  let encode_op tok op =
    (match tok with
     | LET -> "let__"
     | AND -> "and__"
     | _ -> assert false) ^ op

  let wrap (lexer : Lexing.lexbuf -> Parser.token) =
    let pending = Queue.create () in
    let add x = Queue.push x pending in
    fun lb ->
      if not (Queue.is_empty pending) then begin
        let tok, loc = Queue.pop pending in
        restore_loc lb loc;
        tok
      end else
        match lexer lb with
        | LET | AND as tok ->
          let loc = save_loc lb in
          (match Let_trail.op lb with
           | None -> ()
           | Some op ->
             let loc2 = save_loc lb in
             let loc = (fst loc, snd loc2) in
             add (LBRACKETAT, loc);
             add (LIDENT ("!" ^ encode_op tok op), loc);
             add (RBRACKET, loc));
          restore_loc loc;
          tok
        | LPAREN -> begin
            let loc1 = save_loc lb in
            let tok2 = lexer lb in
            let loc2 = save_loc lb in
            add (tok, loc2);
            (match tok with
             | LET | AND -> begin
                 match Let_trail.op lb with
                 | None -> ()
                 | Some op ->
                   let loc3 = save_loc lb in
                   match lexer lb with
                   | RPAREN ->
                     add (LIDENT (encode_op tok op), (fst loc2, snd loc3))
                   | _ ->
                     Location.raise_errorf
                       ~loc:{ loc_ghost = false
                            ; loc_start = fst loc4
                            ; loc_end = snd loc4
                            }
                       "')' expected"
               end
             | _ -> ());
            restore_loc lb loc1;
            LPAREN
          end
end

module Map_ast = struct
  open Ast_mapper
  open Asttypes
  open Parsetree

  let extract_op vb =
    match
      List.find vb.pvb_attributes ~f:(function
        | ({ txt; _ }, PStr []) when txt <> "" && txt.[0] = '!' -> true
        | _ -> false)
    with
    | ({ txt; loc }, _) ->
      let len = String.length txt in
      Some (loc, String.sub txt ~pos:1 ~len:(len - 1))
    | exception Not_found ->
      None

  let mapper =
    let super = default_mapper in
    let map_expression self expr =
      let 
      match expr.pexp_desc with
      | Pexp_let (Nonrec, vb :: vbs, body) -> begin
        match extract_op vb with
    let mapper =
      { super with expression =
                     
    
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
