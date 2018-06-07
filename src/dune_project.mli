(** dune-project files *)

open Import

module Name : sig
  (** Invariants:
      - Named     s -> s <> "" and s does not contain '.' or '/'
      - Anonymous p -> p is a local path in the source tree
  *)
  type t = private
    | Named     of string
    | Anonymous of Path.t

  val compare : t -> t -> Ordering.t

  (** Convert to a string that is suitable for human readable messages *)
  val to_string_hum : t -> string

  val sexp_of_t : t -> Sexp.t

  (** Convert to/from an encoded string that is suitable to use in filenames *)
  val encode : t -> string
  val decode : string -> t
end

type t =
  { name                  : Name.t
  ; root                  : Path.t
  ; version               : string option
  ; packages              : Package.t Package.Name.Map.t
  ; mutable stanza_parser : Stanza.t Sexp.Of_sexp.t
  }

module Lang : sig
  type project = t
  module One_version : sig
    type t

    module Info : sig
      type t

      val make
        :  ?stanzas:(project -> Stanza.t Sexp.Of_sexp.Constructor_spec.t list)
        -> unit
        -> t
    end

    (** [make version args_spec f] defines one version of a
        language. Users will select this language by writing:

        {[ (lang <name> <version> <args>) ]}

        in their [dune-project] file. [args_spec] is used to describe
        what [<args>] might be.
    *)
    val make : Syntax.Version.t -> Info.t -> t
  end

  val register : string -> One_version.t list -> unit
end with type project := t

module Extension : sig
  type project = t

  module One_version : sig
    type t

    module Info : sig
      type t

      val make
        :  ?stanzas:Stanza.t Sexp.Of_sexp.Constructor_spec.t list
        -> unit
        -> t
    end

    (** [make version args_spec f] defines one version of an
        extension. Users will enable this extension by writing:

        {[ (using <name> <version> <args>) ]}

        in their [dune-project] file. [args_spec] is used to describe
        what [<args>] might be.
    *)
    val make
      :  Syntax.Version.t
      -> ('a, Info.t) Sexp.Of_sexp.Constructor_args_spec.t
      -> (project -> 'a)
      -> t
  end

  val register : string -> One_version.t list -> unit
end with type project := t

(** Load a project description from the following directory. [files]
    is the set of files in this directory. *)
val load : dir:Path.t -> files:String.Set.t -> t option

(** "dune-project" *)
val filename : string

(** Represent the scope at the root of the workspace when the root of
    the workspace contains no [dune-project] or [<package>.opam] files. *)
val anonymous : t Lazy.t
