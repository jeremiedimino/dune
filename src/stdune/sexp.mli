include module type of struct include Usexp end with module Loc := Usexp.Loc

module type Combinators = sig
  type 'a t
  val unit       : unit                      t

  val string     : string                    t
  (** Convert an [Atom] or a [Quoted_string] from/to a string. *)

  val int        : int                       t
  val float      : float                     t
  val bool       : bool                      t
  val pair       : 'a t -> 'b t -> ('a * 'b) t
  val triple     : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val list       : 'a t -> 'a list           t
  val array      : 'a t -> 'a array          t
  val option     : 'a t -> 'a option         t

  val string_set : String.Set.t            t
  (** [atom_set] is a conversion to/from a set of strings representing atoms. *)

  val string_map : 'a t -> 'a String.Map.t   t
  (** [atom_map conv]: given a conversion [conv] to/from ['a], returns
     a conversion to/from a map where the keys are atoms and the
     values are of type ['a]. *)

  val string_hashtbl : 'a t -> (string, 'a) Hashtbl.t t
  (** [atom_hashtbl conv] is similar to [atom_map] for hash tables. *)
end

module To_sexp : sig
  type sexp = t
  include Combinators with type 'a t = 'a -> t

  val record : (string * sexp) list -> sexp

  type field

  val field
    :  string
    -> 'a t
    -> ?equal:('a -> 'a -> bool)
    -> ?default:'a
    -> 'a
    -> field
  val field_o : string -> 'a t-> 'a option -> field

  val record_fields : field list t

  val unknown : _ t
end with type sexp := t

module Loc = Usexp.Loc

module Of_sexp : sig
  type ast = Ast.t =
    | Atom of Loc.t * Atom.t
    | Quoted_string of Loc.t * string
    | List of Loc.t * ast list

  type hint =
    { on: string
    ; candidates: string list
    }

  exception Of_sexp of Loc.t * string * hint option

  (** {2 Parsers} *)

  (** Monad for parsing lists *)
  type ('a, 'kind) t

  type simple
  type record

  type 'a simple_parser = ('a, simple) t
  type 'a record_parser = ('a, record) t

  val return : 'a -> ('a, _) t
  val ( >>= )
    :  ('a, 'kind) t
    -> ('a -> ('b, 'kind) t)
    -> ('b, 'kind) t
  val ( >>| )
    :  ('a, 'kind) t
    -> ('a -> 'b)
    -> ('b, 'kind) t

  val simple : (Loc.t -> string -> 'a) -> 'a simple_parser

  (** Return the location of the list being parsed *)
  val loc : (Loc.t, _) t

  val eoi : bool simple_parser

  include Combinators with type 'a t := 'a simple_parser

  val parse : 'a simple_parser -> Ast.t -> 'a

  val of_sexp_error  : ?hint:hint -> Ast.t -> string -> _
  val of_sexp_errorf : ?hint:hint -> Ast.t -> ('a, unit, string, 'b) format4 -> 'a

  val located : 'a simple_parser -> (Loc.t * 'a) simple_parser

  val raw : ast simple_parser

  val string_or_list : (loc:Loc.t -> string -> 'a) -> 'a t -> 'a simple_parser

  val inspect : (Ast.t -> ('a, 'a simple_parser) Either.t) -> 'a simple_parser

  val enum : (string * 'a) list -> 'a simple_parser

  (** {2 Parsing lists} *)

  (** Parser that parse a record, i.e. a list of s-expressions of the
      form [(<atom> <s-exp>)]. *)
  val record : 'a record_parser -> 'a simple_parser

  (** Parser that parse a S-expression of the form [(<atom> <s-exp1>
      <s-exp2> ...)] or [<atom>]. [<atom>] is looked up in the list and
      the remaining s-expressions are parsed using the corresponding
      list parser. *)
  val sum : (string * 'a simple_parser) list -> 'a simple_parser

  (** Parse and consume the rest of the list as a list of element of
      the same type. *)
  val rest : 'a simple_parser -> 'a list simple_parser

  (** Parse all remaining elements as a list of fields *)
  val rest_as_record : 'a record_parser -> 'a simple_parser

  (** Check the result of a list parser, and raise a properly located
      error in case of failure. *)
  val map_validate
    :  'a record_parser
    -> f:('a -> ('b, string) Result.t)
    -> 'b record_parser

  (** {3 Parsing record fields} *)

  module Short_syntax : sig
    type 'a t =
      | Not_allowed
      | This    of 'a
      | Located of (Loc.t -> 'a)
  end

  val field
    :  string
    -> ?short:'a Short_syntax.t
    -> ?default:'a
    -> 'a simple_parser
    -> 'a record_parser
  val field_o
    :  string
    -> ?short:'a Short_syntax.t
    -> 'a simple_parser
    -> 'a option record_parser
  val field_b : string -> bool record_parser

  (** A field that can appear multiple times *)
  val dup_field
    :  string
    -> ?short:'a Short_syntax.t
    -> 'a simple_parser
    -> 'a list record_parser
end

module type Sexpable = sig
  type t
  val t : t Of_sexp.t
  val sexp_of_t : t To_sexp.t
end
