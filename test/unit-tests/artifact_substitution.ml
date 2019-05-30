open Stdune
open Dune

(* Just so that the "dune" language get registered, otherwise
   [Lazy.force Dune_project.anonymous] fails *)
let _ = Dune_file.Stanzas.parse

let () =
  Path.set_root (Path.External.cwd ());
  Path.set_build_dir (Path.Kind.of_string "_build")

(* Test harness

   The test harness takes the input as a symbolic input composed of
   chunks of raw data and substitutions. It then procuces the actual
   input and expected from this symbolic input and checks that the
   substitution algorithm procudes the same answer.
*)

type chunk =
  | Data of string
  | Subst of Artifact_substitution.t

let make l =
  List.map l ~f:(function
    | Data s -> s
    | Subst t -> Artifact_substitution.encode t)
  |> String.concat ~sep:""

let simple_subst l =
  List.map l ~f:(function
    | Data s -> s
    | Subst { mode; value } ->
      match value with
      | Repeat (n, s) ->
        Artifact_substitution.Mode.encode_substitution mode
          (List.init n ~f:(fun _ -> s) |> String.concat ~sep:"")
      | _ ->
        failwith "substitution value not supported")
  |> String.concat ~sep:""

let compress_string s =
  let buf = Buffer.create (String.length s * 2) in
  let chain_length = ref 0 in
  let last_char = ref '\000' in
  let commit_chain () =
    let s = Char.escaped !last_char in
    if !chain_length > 5 then
      Printf.bprintf buf "%s\\{%d}" s !chain_length
    else
      for _i = 1 to !chain_length do
        Buffer.add_string buf s
      done
  in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if c = !last_char then
      incr chain_length
    else begin
      commit_chain ();
      last_char := c;
      chain_length := 1
    end
  done;
  Buffer.contents buf

let test x =
  let input = make x and expected = simple_subst x in
  let buf = Buffer.create (String.length expected) in
  Fiber.run
    (let input =
       let ofs = ref 0 in
       fun buf pos len ->
         let to_copy = min len (String.length input - !ofs) in
         Bytes.blit_string
           ~src:input
           ~dst:buf
           ~src_pos:!ofs
           ~dst_pos:pos
           ~len:to_copy;
         ofs := !ofs + to_copy;
         to_copy
     in
     let output = Buffer.add_subbytes buf in
     Artifact_substitution.copy
       ~file_tree:(Lazy.force File_tree.empty)
       ~input
       ~output);
  let result = Buffer.contents buf in
  if result <> expected then
    Printf.printf "--------------------------\n\
                   ERROR: got invalid result!\n\
                   Input:    \"%s\"\n\
                   Expected: \"%s\"\n\
                   Result:   \"%s\"\n"
      (compress_string input)
      (compress_string expected)
      (compress_string result)

(* Tests *)

let () =
  test [Data ""];
  test [Data "lkdjflskfjlksdf"]

let () =
  test [ Data "foo "
       ; Subst { mode = Text; value = Repeat (2, "xyz") }
       ; Data " bar"
       ]

(* Test various cases of incomplete placeholders *)
let () =
  let s =
    Artifact_substitution.encode { mode = Text; value = Repeat (2, "xyz") }
  in
  let test s = test [Data s] in
  let test i j c =
    let s = String.sub s ~pos:0 ~len:i and data = String.make j c in
    test (data ^ s);
    test (s ^ data);
    test (data ^ s ^ data)
  in
  for i = 0 to String.length s - 1 do
    for j = 0 to 2 do
      test i j 'x'
    done;
    for j = 65536 - i to 65536 + 1 do
      test i j 'x'
    done
  done;
  for i = 0 to String.length s - 3 do
    for j = 0 to 2 do
      test i j '%'
    done;
    for j = 65536 - i to 65536 + 1 do
      test i j '%'
    done
  done
