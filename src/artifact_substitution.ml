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

module Copy_and_substitute = struct
  let buf_len = max_len
  let buf = Bytes.create buf_len

  type copier =
    { input : (Bytes.t -> int -> int -> int)
    ; output : (Bytes.t -> int -> int -> unit)
    }

  type potential_placeholder =
    { (* Offset in [buf] where the placeholder was found *)
      placeholder_start : int
    ; (* Where the data ends in [buf] *)
      end_of_data : int
    ; (* The potential placeholder string itself *)
      placeholder : string
    }

  type scan_result =
    | Found_potential_placeholder of potential_placeholder
    | Scan0
    | Scan1
    | Scan2
    | Scan_prefix of int (* Start of placeholder *)
    | Scan_length of int (* Start of placeholder *) *
                     int (* accumulated length *)

  (* The following functions scan the input channel for potential
     placeholders and copy the contents to the output channel whenever
     they know for sure that the data are not part of a placeholder.

     These functions need to be fast as they will scan every single
     byte of the input.  They only recognize [prefix] and the length
     field.  The rest of the placeholder is parsed using [decode].

     In the future, we expect to have a C version of the scanning
     functions so that it can be delegated to a separate C thread when
     the file to copy is big.  This is why they are written this way
     and in particular this they don't perform the substitution
     themselves.

     When a potential placeholder is found, we return
     [Found_potential_placeholder].  The caller of [scan] is then
     responsible for handling the placeholder and continuing the copy.
     When [Found_potential_placeholder] is returned, the placeholder
     itself is still in [buf] in case it is not a real placeholder.

     In all the [scanXXX] functions, [pos] represent the current
     reading position in [buf].  All the characters between [0] and
     [pos] exclusive have been analysed.  [end_of_data] represents the
     position in [buf] where the data read from the input file end.
     [placeholder_start] represents the position in [buf] where the
     placeholder currently being scanned starts.

     [placeholder_start] is not explicitely passed in [scan0], [scan1]
     and [scan2] since it can easily be deduced from [pos]: it is
     [pos] for [scan0], [pos - 1] for [scan1] and [pos - 2] for
     [scan2].

     [scan_prefix ~pos ~placeholder_start] corresponds to the state
     where the last [pos - placeholder_start] characters in [buf] are
     equal to the first [pos -placeholder_start] characters in
     [prefix].

     [scan_length ~acc] corresponds to the state where we have seen
     [prefix] and are now reading the length, with current accumulated
     value [acc] for the length.

     Note that we use function parameters rather than mutable record
     fields in [copier] so that the values can be kept in registers.
  *)

  let rec scan0 ~pos ~end_of_data =
    if pos < end_of_data then
      match Bytes.unsafe_get buf pos with
      | '%' ->
        scan1 copier
          ~pos:(pos + 1)
          ~end_of_data
      | _ ->
        scan0 copier
          ~pos:(pos + 1)
          ~end_of_data
    else
      Scan0

  and scan1 ~pos ~end_of_data =
    if pos < end_of_data then
      match Bytes.unsafe_get buf pos with
      | '%' ->
        scan2 copier
          ~pos:(pos + 1)
          ~end_of_data
      | _ ->
        scan0 copier
          ~pos:(pos + 1)
          ~end_of_data
    else
      Scan1

  and scan2 ~pos ~end_of_data =
    if pos < end_of_data then
      match Bytes.unsafe_get buf pos with
      | '%' ->
        scan2 copier
          ~pos:(pos + 1)
          ~end_of_data
      | 'D' ->
        scan_prefix copier
          ~pos:(pos + 1)
          ~placeholder_start:(pos - 2)
          ~end_of_data
      | _ ->
        scan0 copier
          ~pos:(pos + 1)
          ~end_of_data
    else
      Scan2

  and scan_prefix ~pos ~placeholder_start ~end_of_data =
    if placeholder_start = prefix_len then
      scan_length ~pos ~placeholder_start ~end_of_data ~acc:0
    else if pos < end_of_data then
      match Bytes.unsafe_get buf pos with
      | '%' ->
        scan1 copier
          ~pos:(pos + 1)
          ~end_of_data
      | c ->
        if c = prefix.[pos - placeholder_start] then
          scan_prefix copier
            ~placeholder_start
            ~pos:(pos + 1)
            ~end_of_data
        else
          scan0 copier
            ~pos:(pos + 1)
            ~end_of_data
    else
      Scan_prefix placeholder_start

  and scan_length ~pos ~placeholder_start ~end_of_data ~acc =
    if pos < end_of_data then
      match Bytes.unsafe_get buf pos with
      | '%' ->
        scan1 copier
          ~pos:(pos + 1)
          ~end_of_data
      | '0'..'9' as c ->
        let n = Char.code c - Char.code '0' in
        let acc = acc * 10 + n in
        if acc = 0 || acc > max_len then
          (* We don't allow leading zeros in length fields and a
             length of [0] is not possible, so [acc = 0] here
             correspond to an invalid placeholder *)
          scan0 copier
            ~pos:(pos + 1)
            ~end_of_data
        else
          scan_length copier
            ~placeholder_start
            ~pos:(pos + 1)
            ~end_of_data
            ~acc
      | ':' ->
        if pos - placeholder_start + String.length ":M:%%" > acc then
          (* If the length is too small, then this is surely not a
             valid placeholder *)
          scan0 copier
            ~pos:(pos + 1)
            ~end_of_data
        else
          Found_potential_placeholder (placeholder_start, acc)
      | _ ->
        scan0 copier
          ~pos:(pos + 1)
          ~end_of_data
    else
      Scan_length (placeholder_start, acc)
 
  (* Refills [buf] by reading from the input channel.  The data
     between [placeholder_start] and [end_of_data] are moved to the
     beginning of [buf] and the new data from the input are placed
     after that.  We need to keep the data between [placeholder_start]
     and [end_of_data] in [buf] until we are sure that we indeed have
     a full placeholder.  If we realise that we don't, then such data
     would need to be sent to the output.

     If the end of input is reached, then we know for sure that these
     data are not the beginning of a placeholder and can be sent to
     the output channel.

     After a call to [refill], [pos] is [end_of_data -
     placeholder_start], [placeholder_start] is reset to [0] and the
     new [end_of_data] position is returned by [refill].

     [refill] is marked with [@@inline never] because it is not called
     often and we don't want it to be inlined in all the [scan]
     functions.
  *)
  let refill copier ~placeholder_start ~end_of_data =
    if placeholder_start > 0 then copier.output buf 0 placeholder_start;
    let leftover = end_of_data - placeholder_start in
    if leftover > 0 then
      Bytes.blit ~src:buf ~dst:buf ~src_pos:placeholder_start ~dst_pos:0
        ~len:leftover;
    match copier.input buf leftover (buf_len - leftover) with
    | 0 ->
      copier.output buf 0 leftover;
      0
    | n ->
      leftover + n
  [@@inline never]

  let rec loop ft copier ~pos ~end_of_data =
    match scan0 copier ~pos ~end_of_data with
    | Found_potential_placeholder (placeholder_start, len) -> begin
        if end_of_data - placeholder_start >= len then
          process_placeholder ft copier ~placeholder_start ~len
            ~end_of_data:n
        else
          match refill copier ~placeholder_start ~end_of_data with
          | 0 -> Fiber.return ()
          | n ->
            if n >= len then
              process_placeholder ft copier ~placeholder_start:0 ~len
                ~end_of_data:n
            else begin
              copier.output buf 0 n;
              Fiber.return ()
            end
      end
    | st ->
      match refill copier ~placeholder_start ~end_of_data with
      | 0 -> Fiber.return ()
      | n ->
        
      
  and process_placeholder ft copier ~placeholder_start ~len ~end_of_data =
    let open Fiber.O in
    let placeholder = Bytes.sub_string buf placeholder_start len in
    match decode placeholder with
    | Some t ->
      let* v = eval ft t in
      copier.output buf 0 placeholder_start;
      let s = Mode.encode_substitution t.mode v in
      copier.output (Bytes.unsafe_of_string s) 0 (String.length s);
      loop ft copier
        ~pos:(placeholder_start + len)
        ~end_of_data
    | None ->
      (* Restart just after [prefix] since we know for sure that a
         placeholder cannot start before that. *)
      loop ft copier
        ~pos:(placeholder_start + prefix_len)
        ~end_of_data

  let run ft copier = loop ft copier ~pos:0 ~end_of_data:0
end

let copy ~file_tree ~input ~output () =
  Copy_and_substitute.run file_tree { input; output }

let copy_file ~file_tree ?(chmod=Fn.id) ~src ~dst () =
  Io.with_file_in src ~f:(fun ic ->
    let perm = (Unix.fstat (Unix.descr_of_in_channel ic)).st_perm |> chmod in
    Exn.protectx (Pervasives.open_out_gen
                    [Open_wronly; Open_creat; Open_trunc; Open_binary]
                    perm
                    (Path.to_string dst))
      ~finally:close_out
      ~f:(fun oc ->
        copy ~file_tree ~input:(input ic) ~output:(output oc) () ))
