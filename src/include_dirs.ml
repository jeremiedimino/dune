open Stdune

type t =
  { seen : Path.Set.t
  ; rev_dirs : Path.t list
  }

let empty ~implicit_dirs =
  { seen = implicit_dirs
  ; rev_dirs = []
  }

let get t = List.rev t.rev_dirs

let to_iflags t =
  Arg_spec.S
    (List.fold_left t.rev_dirs ~init:[] ~f:(fun acc dir ->
       Arg_spec.A "-I" :: Path dir :: acc))

let append t dir =
  if Path.Set.mem t.seen dir then
    t
  else
    { seen = Path.Set.add t.seen dir
    ; rev_dirs = dir :: t.rev_dirs
    }

let concat a b =
  List.fold_left b.rev_dirs ~init:a ~f:append
