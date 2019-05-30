open Import

(* Artifact substitutions works as follow: the substitution is encoded
   as a string of the form:

   {v %%DUNE_PLACEHOLDER:<len>:<mode>:<symbolic-value>%% v}

   Where:
   - [<len>] is the full length of the encoded string
   - [<mode>] is [t] for [Text] and [b] for [Binary]
   - [<symbolic-value>] is an encoding of a [Value.t]

   For instance:

   {v %%DUNE_PLACEHOLDER:48:t:vcs-describe:7:src/foo%% v}

   When the mode is [Fixed_length n], the substitution is padded with
   '%' characters to make it of length exactly [n].

   The [copy_file] functions recognises such strings and substitute
   them accordingly.  *)

module Mode = struct
  type t =
    | Text
    | Binary of int

  let to_sexp = function
    | Binary n -> Sexp.Encoder.constr "Binary" [Sexp.Encoder.int n]
    | Text -> Sexp.Encoder.constr "Text" []

  let encode_substitution t s =
    match t with
    | Text -> s
    | Binary len ->
      let s =
        if String.length s + 3 >= len then
          String.sub s ~pos:0 ~len:(len - 3)
        else
          s
      in
      let str_len = String.length s in
      sprintf "\000%c%c%s%s"
        (Char.chr (str_len lsr 8))
        (Char.chr (str_len land 0xff))
        s
        (String.make (len - 3 - str_len) '\000')
end

module Value = struct
  type t =
    | Vcs_describe of Path.Source.t
    | Repeat of int * string

  let to_sexp = function
    | Vcs_describe p ->
      Sexp.Encoder.constr "Vcs_describe" [Path.Source.to_sexp p]
    | Repeat (n, s) ->
      Sexp.Encoder.constr "Repeat" [Sexp.Encoder.int n; Atom s]
end

type t =
  { mode : Mode.t
  ; value : Value.t
  }

let to_sexp { mode; value }=
  Sexp.Encoder.record
    [ "mode", Mode.to_sexp mode
    ; "value", Value.to_sexp value
    ]

let prefix = "%%DUNE_PLACEHOLDER:"
let prefix_len = String.length prefix
let max_len = 0xffff

let encode { mode; value } =
  let suffix =
    sprintf ":%c:%s%%%%"
      (match mode with
       | Binary _ -> 'f'
       | Text -> 'v')
      (match value with
       | Vcs_describe p ->
         let s = Path.Source.to_string p in
         sprintf "vcs-describe:%d:%s" (String.length s) s
       | Repeat (n, s) ->
         sprintf "repeat:%d:%d:%s" n (String.length s) s)
  in
  let partial_len = prefix_len + String.length suffix in
  let partial_len =
    match mode with
    | Binary n -> max n partial_len
    | Text -> partial_len
  in
  let len =
    let partial_len_str = string_of_int partial_len in
    let len = partial_len + String.length partial_len_str in
    if String.length (string_of_int len) = String.length partial_len_str then
      len
    else
      len + 1
  in
  if len < 0 || len > max_len then
    Exn.code_error
      "Artifact_substitution.encode: length is too large"
      [ "t", to_sexp { mode; value }
      ; "len", Sexp.Encoder.int len
      ];
  let s = sprintf "%s%u%s" prefix len suffix in
  let s = s ^ String.make (len - String.length s) '%' in
  assert (String.length s = len);
  s

let eval ft t =
  match t.value with
  | Repeat (n, s) ->
    Fiber.return (Array.make n s |> Array.to_list |> String.concat ~sep:"")
  | Vcs_describe p ->
    match File_tree.Dir.vcs (File_tree.nearest_dir ft p) with
    | None -> Fiber.return "<no-vcs-info>"
    | Some vcs -> Vcs.describe vcs

(* This function is not called very often, so the focus is on
   readibility rather than speed. *)
let decode s =
  let fail () = raise_notrace Exit in
  let parse_int s = try int_of_string s with _ -> fail () in
  let len = String.length s in
  match
    if len < 4 ||
       s.[0] <> '%' ||
       s.[1] <> '%' ||
       s.[len - 2] <> '%' ||
       s.[len - 1] <> '%' then
      fail ();
    let dune_placeholder, len_str, mode_str, rest =
      match String.split (String.sub s ~pos:2 ~len:(len - 4)) ~on:':' with
      | dune_placeholder :: len_str :: mode_str :: rest ->
        (dune_placeholder, len_str, mode_str, rest)
      | _ -> fail ()
    in
    if dune_placeholder <> "DUNE_PLACEHOLDER" then fail ();
    if parse_int len_str <> len then fail ();
    let mode =
      match mode_str with
      | "t" -> Mode.Text
      | "b" -> Mode.Binary len
      | _ -> fail ()
    in
    let read_string_payload = function
      | [] -> fail ()
      | len :: rest ->
        let len = parse_int len in
        let s = String.concat rest ~sep:":" in
        for i = len to String.length s - 1 do
          if s.[i] <> '%' then fail ()
        done;
        String.sub s ~pos:0 ~len
    in
    let value : Value.t =
      match rest with
      | "vcs-describe" :: rest ->
        let path = Path.Source.of_string (read_string_payload rest) in
        Vcs_describe path
      | "repeat" :: repeat :: rest ->
        Repeat (parse_int repeat, read_string_payload rest)
      | _ -> fail ()
    in
    { mode; value }
  with
  | exception Exit -> None
  | t -> Option.some_if (encode t = s) t

(* Scan a buffer for "%%DUNE_PLACEHOLDER:<len>:" *)
module Scanner = struct
  (* The following module implement a scanner for special placeholders
     strings.  The scanner needs to be fast as possible as it will
     scan every single byte of the input so this module is carefully
     written with performances in mind.

     The logic is implemented as a DFA where each OCaml function
     represent a state of the automaton.

     In the future, we expect to have a C version of the scanner so
     that it can run concurrently with OCaml code in a separate
     thread.  This will speed up the promotion of large binaries from
     the build directory to the source tree.

     {1 Notations}

     In this module, [buf] is the buffer containing the data read from
     the input.  [end_of_data] is the position in [buf] of the end of
     data read from the input.  [pos] is the current reading position
     in [buf].  [placeholder_start] represents the position in [buf]
     where the current potential placeholder starts in [buf].  *)

  (* The following type represents the possible state of the DFA. *)
  type state =
    | Scan0
    (* Initial state and state when we are not in a potential placeholder *)
    | Scan1
    (* State after seeing one '%' *)
    | Scan2
    (* State after seeing at least two '%' *)
    | Scan_prefix of int
    (* [Scan_prefix placeholer_start] is the state after seeing [pos -
       placeholer_start] characters from [prefix] *)
    | Scan_length of int * int
    (* [Scan_length (placeholer_start, acc)] is the state after seeing
       all of [prefix] and the beginning of the length field. [acc] is
       the length accumulated so far. *)
    | Scan_placeholder of int * int
    (* [Scan_placeholder (placeholer_start, len)] is the state after
       seeing all of [prefix] and the length field, i.e. just after
       the second ':' of the placeholder *)

  (* The [run] function at the end of this module is the main function
     that consume a buffer and return the new DFA state.  If the
     beginning of placeholder is found before reaching the end of the
     buffer, [run] immediately returns [Scan_placeholder
     (placeholder_start, len)].

     The following functions represent the transition functions for
     each state of the DFA.  [run] is called only at the beginning and
     immediately chain the execution to the right transition
     function.

     Each transition function is written as follow:

     {[
       let state ~buf ~pos ~end_of_data ... =
         if pos < end_of_data then
           let c = Bytes.unsafe_get buf pos in
           let pos = pos + 1 in
           match c with
           ...
         else
           State (...)
     ]}

     i.e. it either inspect the next character and chain the execution to the
     next transition function or immediately return the DFA state if the end of
     buffer is reached.  Reaching the end of buffer is handled in the [else]
     branch for performance reason: the code generated by OCaml tend to make
     following the [then] branch slightly more efficient and reaching the
     end of buffer is the less likely case.
  *)

  let rec scan0 ~buf ~pos ~end_of_data =
    if pos < end_of_data then
      let c = Bytes.unsafe_get buf pos in
      let pos = pos + 1 in
      match c with
      | '%' ->
        scan1
          ~buf
          ~pos
          ~end_of_data
      | _ ->
        scan0
          ~buf
          ~pos
          ~end_of_data
    else
      Scan0

  and scan1 ~buf ~pos ~end_of_data =
    if pos < end_of_data then
      let c = Bytes.unsafe_get buf pos in
      let pos = pos + 1 in
      match c with
      | '%' ->
        scan2
          ~buf
          ~pos
          ~end_of_data
      | _ ->
        scan0
          ~buf
          ~pos
          ~end_of_data
    else
      Scan1

  and scan2 ~buf ~pos ~end_of_data =
    if pos < end_of_data then
      let c = Bytes.unsafe_get buf pos in
      let pos = pos + 1 in
      match c with
      | '%' ->
        scan2
          ~buf
          ~pos
          ~end_of_data
      | 'D' ->
        scan_prefix
          ~buf
          ~pos
          ~end_of_data
          ~placeholder_start:(pos - 3)
      | _ ->
        scan0
          ~buf
          ~pos
          ~end_of_data
    else
      Scan2

  and scan_prefix ~buf ~pos ~end_of_data ~placeholder_start =
    if pos < end_of_data then
      let pos_in_prefix = pos - placeholder_start in
      let c = Bytes.unsafe_get buf pos in
      let pos = pos + 1 in
      match c with
      | '%' ->
        scan1
          ~buf
          ~pos
          ~end_of_data
      | c ->
        if c = prefix.[pos_in_prefix] then begin
          if pos_in_prefix = prefix_len - 1 then
            scan_length
              ~buf
              ~pos
              ~end_of_data
              ~placeholder_start
              ~acc:0
          else
            scan_prefix
              ~buf
              ~pos
              ~end_of_data
              ~placeholder_start
        end else
          scan0
            ~buf
            ~pos
            ~end_of_data
    else
      Scan_prefix placeholder_start

  and scan_length ~buf ~pos ~end_of_data ~placeholder_start ~acc =
    if pos < end_of_data then
      let c = Bytes.unsafe_get buf pos in
      let pos = pos + 1 in
      match c with
      | '%' ->
        scan1
          ~buf
          ~pos
          ~end_of_data
      | '0'..'9' as c ->
        let n = Char.code c - Char.code '0' in
        let acc = acc * 10 + n in
        if acc = 0 || acc > max_len then
          (* We don't allow leading zeros in length fields and a
             length of [0] is not possible, so [acc = 0] here
             correspond to an invalid placeholder *)
          scan0
            ~buf
            ~pos
            ~end_of_data
        else
          scan_length
            ~buf
            ~pos
            ~end_of_data
            ~placeholder_start
            ~acc
      | ':' ->
        if pos - placeholder_start + String.length ":M:%%" > acc then
          (* If the length is too small, then this is surely not a
             valid placeholder *)
          scan0
            ~buf
            ~pos
            ~end_of_data
        else
          Scan_placeholder (placeholder_start, acc)
      | _ ->
        scan0
          ~buf
          ~pos
          ~end_of_data
    else
      Scan_length (placeholder_start, acc)

  let run state ~buf ~pos ~end_of_data =
    match state with
    | Scan0 -> scan0 ~buf ~pos ~end_of_data
    | Scan1 -> scan1 ~buf ~pos ~end_of_data
    | Scan2 -> scan2 ~buf ~pos ~end_of_data
    | Scan_prefix placeholder_start ->
      scan_prefix ~buf ~pos ~end_of_data ~placeholder_start
    | Scan_length (placeholder_start, acc) ->
      scan_length ~buf ~pos ~end_of_data ~placeholder_start ~acc
    | Scan_placeholder _ ->
      state
end

let copy =
  let buf_len = max_len in
  let buf = Bytes.create buf_len in
  fun ~file_tree ~input ~output ->
    let open Fiber.O in
    let rec loop scanner_state ~pos ~end_of_data =
      match Scanner.run scanner_state ~buf ~pos ~end_of_data with
      | Scan_placeholder (placeholder_start, len)
        when len <= end_of_data - placeholder_start -> begin
          let placeholder = Bytes.sub_string buf ~pos:placeholder_start ~len in
          match decode placeholder with
          | Some t ->
            let* v = eval file_tree t in
            let s = Mode.encode_substitution t.mode v in
            output buf 0 placeholder_start;
            output (Bytes.unsafe_of_string s) 0 (String.length s);
            loop
              Scan0
              ~pos:(placeholder_start + len)
              ~end_of_data
          | None ->
            (* Restart just after [prefix] since we know for sure that
               a placeholder cannot start before that. *)
            loop
              Scan0
              ~pos:(placeholder_start + prefix_len)
              ~end_of_data
        end
      | scanner_state ->
        let placeholder_start =
          match scanner_state with
          | Scan0 -> end_of_data
          | Scan1 -> end_of_data - 1
          | Scan2 -> end_of_data - 2
          | Scan_prefix placeholder_start
          | Scan_length (placeholder_start, _)
          | Scan_placeholder (placeholder_start, _) -> placeholder_start
        in
        (* All the data before [placeholder_start] can be sent to the
           output immediately since we know for sure that it is not
           part of a placeholder *)
        if placeholder_start > 0 then output buf 0 placeholder_start;
        (* [leftover] correspond to a prefix of a potential
           placeholder in [buf].  We need to keep it in [buf] until we
           know for sure whether it is a real placeholder or not. *)
        let leftover = end_of_data - placeholder_start in
        if leftover > 0 then
          Bytes.blit ~src:buf ~dst:buf ~src_pos:placeholder_start ~dst_pos:0
            ~len:leftover;
        match input buf leftover (buf_len - leftover) with
        | 0 ->
          (* Nothing more to read; [leftover] is definitely not the
             beginning of a placeholder, send it and end the operation
          *)
          output buf 0 leftover;
          Fiber.return ()
        | n ->
          loop scanner_state
            ~pos:leftover
            ~end_of_data:(leftover + n)
    in
    loop Scan0 ~pos:0 ~end_of_data:0

let copy_file ~file_tree ?(chmod=Fn.id) ~src ~dst () =
  Io.with_file_in src ~f:(fun ic ->
    let perm = (Unix.fstat (Unix.descr_of_in_channel ic)).st_perm |> chmod in
    Exn.protectx (Pervasives.open_out_gen
                    [Open_wronly; Open_creat; Open_trunc; Open_binary]
                    perm
                    (Path.to_string dst))
      ~finally:close_out
      ~f:(fun oc ->
        copy ~file_tree ~input:(input ic) ~output:(output oc)))
