open StdLabels

module CS = Set.Make(Char)

type item =
  | Char of CS.t
  | Rep  of CS.t
  | Start
  | Stop
  | Too_complicated

type t = item list

let start = [Start]
let stop = [Stop]

let any =
  let rec loop acc i =
    if i = 256 then
      [Char acc]
    else
      loop (CS.add (Char.chr i) acc) (i + 1)
  in
  loop CS.empty 0

let char c = [Char (CS.singleton c)]
let set s =
  let res = ref CS.empty in
  for i = 0 to String.length s - 1 do
    res := CS.add s.[i] !res
  done;
  [Char !res]

let diff a b =
  match a, b with
  | [Char a], [Char b] -> [Char (CS.diff a b)]
  | _ -> invalid_arg "Re.diff"

let str s =
  let len = String.length s in
  let rec loop i =
    if i = len then
      []
    else
      Char (CS.singleton s.[i]) :: loop (i + 1)
  in
  loop 0

let alt _ = [Too_complicated]

let seq = List.concat
let rep = function
  | [Char cs] -> [Rep cs]
  | _ -> [Too_complicated]

(* Encode S-expressions of the form [prefix repeat* suffix] where
   prefix and suffix are fixed length. *)
type re =
  { prefix  : CS.t array
  ; repeat  : CS.t option
  ; suffix  : CS.t array
  }

let compile t =
  let rec extract_side acc = function
    | Char cs :: l -> extract_side (cs :: acc) l
    | l -> (acc, [])
  in
  let t =
    match t with
    | Start :: t -> t
    | _ -> t
  in
  let prefix, t = extract_side [] t in
  let t =
    match List.rev t with
    | Stop :: t -> t
    | _ -> t
  in
  let suffix, t = extract_side [] t in
  let repeat =
    match List.rev t with
    | [] -> None
    | [Rep cs] -> Some cs
    | _ -> raise Exit
  in
  { prefix = Array.of_list (List.rev prefix)
  ; suffix = Array.of_list suffix
  ; repeat
  }

let fail () = raise_notrace Exit

let execp re s =
  try
    let len = String.length s in
    let prefix_len = Array.length re.prefix in
    let suffix_len = Array.length re.suffix in
    if len < prefix_len + suffix_len then fail ();
    let suffix_start = len - suffix_len in
    for i = 0 to prefix_len - 1 do
      if not (CS.mem s.[i] re.prefix.(i)) then fail ()
    done;
    (match re.repeat with
     | None -> if prefix_len <> suffix_start then fail ()
     | Some cs ->
       for i = prefix_len to suffix_start - 1 do
         if not (CS.mem s.[i] cs) then fail ()
       done);
    for i = suffix_start to len - 1 do
      if not (CS.mem s.[i] re.suffix.(i - suffix_start)) then fail ()
    done;
    true
  with Exit ->
    false
