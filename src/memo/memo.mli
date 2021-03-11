open! Stdune

module Build : sig
  (** The build monad *)

  type 'a t

  val run : 'a t -> 'a Fiber.t

  (** [of_reproducible_fiber fiber] injects a fiber into the build monad. This
      module assumes that the given fiber is "reproducible", i.e. that executing
      it multiple times will always yield the same result.

      It is however up to the user to ensure this property. *)
  val of_reproducible_fiber : 'a Fiber.t -> 'a t

  val return : 'a -> 'a t

  module O : sig
    val ( >>> ) : unit t -> 'a t -> 'a t

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  val both : 'a t -> 'b t -> ('a * 'b) t

  val sequential_map : 'a list -> f:('a -> 'b t) -> 'b list t

  val sequential_iter : 'a list -> f:('a -> unit t) -> unit t

  val fork_and_join : (unit -> 'a t) -> (unit -> 'b t) -> ('a * 'b) t

  val fork_and_join_unit : (unit -> unit t) -> (unit -> 'a t) -> 'a t

  val parallel_map : 'a list -> f:('a -> 'b t) -> 'b list t

  val parallel_iter : 'a list -> f:('a -> unit t) -> unit t

  val parallel_iter_set :
       (module Set.S with type elt = 'a and type t = 's)
    -> 's
    -> f:('a -> unit t)
    -> unit t

  module Make_map_traversals (Map : Map.S) : sig
    val parallel_iter : 'a Map.t -> f:(Map.key -> 'a -> unit t) -> unit t

    val parallel_map : 'a Map.t -> f:(Map.key -> 'a -> 'b t) -> 'b Map.t t
  end
  [@@inline always]

  (** The bellow functions will eventually disappear and are only exported for
      the transition to the memo monad *)

  val with_error_handler :
    (unit -> 'a t) -> on_error:(Exn_with_backtrace.t -> unit) -> 'a t

  val fold_errors :
       (unit -> 'a t)
    -> init:'b
    -> on_error:(Exn_with_backtrace.t -> 'b -> 'b)
    -> ('a, 'b) Result.t t

  val collect_errors :
    (unit -> 'a t) -> ('a, Exn_with_backtrace.t list) Result.t t

  val finalize : (unit -> 'a t) -> finally:(unit -> unit t) -> 'a t

  val reraise_all : Exn_with_backtrace.t list -> 'a t
end

type ('input, 'output, 'f) t

(* Temporary type to forbid use of sync memo until we have simplified the
   implementation. *)
type 'a forbidden

module Sync : sig
  type nonrec ('i, 'o) t = ('i, 'o, ('i -> 'o) forbidden) t
end

module Async : sig
  type nonrec ('i, 'o) t = ('i, 'o, 'i -> 'o Build.t) t
end

(** A stack frame within a computation. *)
module Stack_frame : sig
  type ('input, 'output, 'f) memo = ('input, 'output, 'f) t

  type t

  val to_dyn : t -> Dyn.t

  val name : t -> string option

  val input : t -> Dyn.t

  (** Checks if the stack frame is a frame of the given memoized function and if
      so, returns [Some i] where [i] is the argument of the function. *)
  val as_instance_of : t -> of_:('input, _, _) memo -> 'input option
end

module Cycle_error : sig
  type t

  exception E of t

  (** Get the list of stack frames in this cycle. *)
  val get : t -> Stack_frame.t list

  (** Return the stack leading to the node which raised the cycle. *)
  val stack : t -> Stack_frame.t list
end

(** Notify the memoization system that the build system has restarted. This
    removes the values that depend on the [current_run] from the memoization
    cache, and cancels all pending computations. *)
val reset : unit -> unit

(** Notify the memoization system that the build system has restarted but do not
    clear the memoization cache. *)
val restart_current_run : unit -> unit

module Function : sig
  module Type : sig
    type ('a, 'b, 'f) t =
      | Sync : ('a, 'b, ('a -> 'b) forbidden) t
      | Async : ('a, 'b, 'a -> 'b Build.t) t
  end

  module Info : sig
    type t =
      { name : string
      ; doc : string option
      }
  end
end

module type Output_simple = sig
  type t

  val to_dyn : t -> Dyn.t
end

module type Output_allow_cutoff = sig
  type t

  val to_dyn : t -> Dyn.t

  val equal : t -> t -> bool
end

(** When we recompute the function and find that its output is the same as what
    we computed before, we can sometimes skip recomputing the values that depend
    on it.

    [Allow_cutoff] specifies how to compare the output values for that purpose.

    Note that currently Dune wipes all memoization caches on every run, so
    cutoff is not effective. *)
module Output : sig
  type 'o t =
    | Simple of (module Output_simple with type t = 'o)
    | Allow_cutoff of (module Output_allow_cutoff with type t = 'o)
end

module type Input = sig
  type t

  include Table.Key with type t := t
end

module Visibility : sig
  type 'i t =
    | Hidden
    | Public of 'i Dune_lang.Decoder.t
end

module Store : sig
  module type Input = sig
    type t

    val to_dyn : t -> Dyn.t
  end

  module type S = sig
    type key

    type 'a t

    val create : unit -> _ t

    val clear : _ t -> unit

    val set : 'a t -> key -> 'a -> unit

    val find : 'a t -> key -> 'a option
  end
end

val create_with_store :
     string
  -> store:(module Store.S with type key = 'i)
  -> ?doc:string
  -> input:(module Store.Input with type t = 'i)
  -> visibility:'i Visibility.t
  -> output:'o Output.t
  -> ('i, 'o, 'f) Function.Type.t
  -> 'f
  -> ('i, 'o, 'f) t

(** [create name ~doc ~input ~visibility ~output f_type f] creates a memoized
    version of [f]. The result of [f] for a given input is cached, so that the
    second time [exec t x] is called, the previous result is re-used if
    possible.

    [exec t x] tracks what calls to other memoized function [f x] performs. When
    the result of such dependent call changes, [exec t x] will automatically
    recompute [f x].

    Running the computation may raise [Memo.Cycle_error.E] if a cycle is
    detected.

    Both simple functions (synchronous) and functions returning fibers
    (asynchronous ones) can be memoized, and the flavor is selected by [f_type].

    [visibility] determines whether the function is user-facing or internal and
    if it's user-facing then how to parse the values written by the user. *)
val create :
     string
  -> ?doc:string
  -> input:(module Input with type t = 'i)
  -> visibility:'i Visibility.t
  -> output:'o Output.t
  -> ('i, 'o, 'f) Function.Type.t
  -> 'f
  -> ('i, 'o, 'f) t

val create_hidden :
     string
  -> ?doc:string
  -> input:(module Input with type t = 'i)
  -> ('i, 'o, 'f) Function.Type.t
  -> 'f
  -> ('i, 'o, 'f) t

(** The call [peek_exn t i] registers a dependency on [t i] and returns its
    value, failing if the value has not yet been computed. We do not expose
    [peek] because the [None] case is hard to reason about, and currently there
    are no use-cases for it. *)
val peek_exn : ('i, 'o, _) t -> 'i -> 'o

(** Execute a memoized function *)
val exec : (_, _, 'f) t -> 'f

(** After running a memoization function with a given name and input, it is
    possible to query which dependencies that function used during execution by
    calling [get_deps] with the name and input used during execution.

    Returns [None] if the dependencies were not computed yet. *)
val get_deps : ('i, _, _) t -> 'i -> (string option * Dyn.t) list option

(** Print the memoized call stack during execution. This is useful for debugging
    purposes. *)
val dump_stack : unit -> unit

val pp_stack : unit -> _ Pp.t

(** Get the memoized call stack during the execution of a memoized function. *)
val get_call_stack : unit -> Stack_frame.t list

(** Call a memoized function by name *)
val call : string -> Dune_lang.Ast.t -> Dyn.t Build.t

module Run : sig
  (** A single build run. *)
  type t

  (** A forward declaration that is reset after every run. *)
  module Fdecl : sig
    type 'a t

    (** [create to_dyn] creates a forward declaration. The [to_dyn] parameter is
        used for reporting errors in [set] and [get]. *)
    val create : ('a -> Dyn.t) -> 'a t

    (** [set t x] sets the value that is returned by [get t] to [x]. Raises if
        [set] was already called. *)
    val set : 'a t -> 'a -> unit

    (** [get t] returns the [x] if [set comp x] was called. Raises if [set] has
        not been called yet. *)
    val get : 'a t -> 'a
  end
end

(** Introduces a dependency on the current build run. *)
val current_run : unit -> Run.t

(** Return the list of registered functions *)
val registered_functions : unit -> Function.Info.t list

(** Lookup function's info *)
val function_info : string -> Function.Info.t

module Lazy : sig
  type +'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  val create :
    (   ?cutoff:('a -> 'a -> bool)
     -> ?to_dyn:('a -> Dyn.t)
     -> (unit -> 'a)
     -> 'a t)
    forbidden

  val of_val : 'a -> 'a t

  val force : 'a t -> 'a

  module Async : sig
    type 'a t

    val of_val : 'a -> 'a t

    val create :
         ?cutoff:('a -> 'a -> bool)
      -> ?to_dyn:('a -> Dyn.t)
      -> (unit -> 'a Build.t)
      -> 'a t

    val force : 'a t -> 'a Build.t

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end
end

val lazy_ :
  (   ?cutoff:('a -> 'a -> bool)
   -> ?to_dyn:('a -> Dyn.t)
   -> (unit -> 'a)
   -> 'a Lazy.t)
  forbidden

val lazy_async :
     ?cutoff:('a -> 'a -> bool)
  -> ?to_dyn:('a -> Dyn.t)
  -> (unit -> 'a Build.t)
  -> 'a Lazy.Async.t

module Implicit_output : sig
  type 'o t

  (** [produce] and [produce_opt] are used by effectful functions to produce
      output. *)
  val produce : 'o t -> 'o -> unit

  val produce_opt : 'o t -> 'o option -> unit

  (** [collect*] and [forbid*] take a potentially effectful function (one which
      may produce some implicit output) and turn it into a pure one (with
      explicit output if any) *)
  val collect_async : 'o t -> (unit -> 'a Build.t) -> ('a * 'o option) Build.t

  val collect_sync : 'o t -> (unit -> 'a) -> 'a * 'o option

  val forbid_async : (unit -> 'a Build.t) -> 'a Build.t

  val forbid_sync : (unit -> 'a) -> 'a

  module type Implicit_output = sig
    type t

    val name : string

    val union : t -> t -> t
  end

  (** Register a new type of implicit output. *)
  val add : (module Implicit_output with type t = 'o) -> 'o t
end

module With_implicit_output : sig
  type ('i, 'o, 'f) t

  val create :
       string
    -> ?doc:string
    -> input:(module Input with type t = 'i)
    -> visibility:'i Visibility.t
    -> output:(module Output_simple with type t = 'o)
    -> implicit_output:'io Implicit_output.t
    -> ('i, 'o, 'f) Function.Type.t
    -> 'f
    -> ('i, 'o, 'f) t

  val exec : (_, _, 'f) t -> 'f
end

module Cell : sig
  type ('a, 'b, 'f) t

  val input : ('a, _, _) t -> 'a

  val get_sync : (('a, 'b, 'a -> 'b) t -> 'b) forbidden

  val get_async : ('a, 'b, 'a -> 'b Build.t) t -> 'b Build.t

  (** Mark this cell as invalid, forcing recomputation of this value. The
      consumers may be recomputed or not, depending on early cutoff. *)
  val invalidate : _ t -> unit
end

val cell : ('a, 'b, 'f) t -> 'a -> ('a, 'b, 'f) Cell.t

(** Memoization of polymorphic functions. When using both [Sync] and [Async]
    modules, the provided [id] function must be injective, i.e. there must be a
    one-to-one correspondence between [input]s and their [id]s. *)
module Poly : sig
  (** Memoization of functions of type ['a input -> 'a output]. *)
  module Sync (Function : sig
    type 'a input

    type 'a output

    val name : string

    val eval : 'a input -> 'a output

    val to_dyn : _ input -> Dyn.t

    val id : 'a input -> 'a Type_eq.Id.t
  end) : sig
    val eval : ('a Function.input -> 'a Function.output) forbidden
  end

  (** Memoization of functions of type ['a input -> 'a output Build.t]. *)
  module Async (Function : sig
    type 'a input

    type 'a output

    val name : string

    val eval : 'a input -> 'a output Build.t

    val to_dyn : _ input -> Dyn.t

    val id : 'a input -> 'a Type_eq.Id.t
  end) : sig
    val eval : 'a Function.input -> 'a Function.output Build.t
  end
end

val unwrap_exn : (exn -> exn) ref

(** If [true], this module will record the location of [Lazy.t] values. This is
    a bit expensive to compute, but it helps debugging. *)
val track_locations_of_lazy_values : bool ref
