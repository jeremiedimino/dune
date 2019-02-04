(** Include directories *)

open Stdune

(** Represent an ordered sequence of directories where each directory
    appears at most once and where the standard library directory never
    appears since it is implicitely added by the compiler. *)
type t

(** An empty sequence of directoryes. [implicit_dirs] are directories
    implicitely added by the compiler. Implicit directories never
    appear in the final sequence. *)
val empty : implicit_dirs:Path.Set.t -> t

(** Return the ordered list of paths. *)
val get : t -> Path.t list

(** Same as [get] but return a list of [-I <dir>] arguments. *)
val to_iflags : t -> _ Arg_spec.t

(** Add a directory at the end of the sequence. If the directory is
    already present, the sequence is returned as it. *)
val append : t -> Path.t -> t

(** Concatenate two sequences. *)
val concat : t -> t -> t
