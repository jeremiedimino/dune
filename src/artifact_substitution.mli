(** Handling of substitutions in artifacts during promotion and installation *)

open Stdune

module Mode : sig
  type t =
    | Text
    (** In text mode, the whole placeholder is replaced by its
        evaluation.  As a result, the new file might be smaller or
        larger than the original one. *)
    | Binary of int
    (** In this mode, the whole placeholder is replaced by a string of
        exactly the same length.  The replacement starts with a null
        character followed by a 16 bits length in big endian and then
        the value.  The rest of the space is padded with null bytes. *)

  (** Produce the string that would replace the placeholder with the
      given value .*)
  val encode_substitution : t -> string -> string
end

(** A symbolic representation of the value to substitute to *)
module Value : sig
  type t =
    | Vcs_describe of Path.Source.t
    | Repeat of int * string
    (** [Repeat (n, s)] evaluates to [s] repeated [n] times. This
        substitution is used for unit tests. *)
end

type t =
  { mode : Mode.t
  ; value : Value.t
  }

(** A string encoding of a substitution. The resulting string is what
    should be written inside generated source files. {!copy_file}
    recognise such strings and expand them. *)
val encode : t -> string

(** [decode s] returns the value [t] such that [encode t = s]. *)
val decode : string -> t option

(** Copy a file, performing all required substitutions *)
val copy_file
  :  file_tree:File_tree.t
  -> ?chmod:(int -> int)
  -> src:Path.t
  -> dst:Path.t
  -> unit
  -> unit Fiber.t

(** Generic version of [copy_file].  Rather than filenames, it takes
    an input and output functions.  Their semantic must match the ones
    of the [input] and [output] functions from the OCaml standard
    library. *)
val copy
  :  file_tree:File_tree.t
  -> input:(Bytes.t -> int -> int -> int)
  -> output:(Bytes.t -> int -> int -> unit)
  -> unit
  -> unit Fiber.t
