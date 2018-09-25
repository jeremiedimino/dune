(** Concurrency library *)

open! Stdune

(** {1 Generals} *)

(** Type of fiber. A fiber represent a suspended computation. Note
    that using the same fiber twice will execute it twice, which is
    probably not what you want. To share the result of a fiber, use an
    [Ivar.t].  *)
type ('a, 'context) t

(** Create a fiber that has already terminated. *)
val return : 'a -> ('a, _) t

(** Fiber that never completes. *)
val never : ('a, _) t

module O : sig
  (** [>>>] is a sequencing operator. [a >>> b] is the fiber that
      first executes [a] and then [b]. *)
  val (>>>) : (unit, 'ctx) t -> ('a, 'ctx) t -> ('a, 'ctx) t

  (** [>>=] is similar to [>>>] except that the result of the first
      fiber is used to create the second one. *)
  val (>>=) : ('a, 'ctx) t -> ('a -> ('b, 'ctx) t) -> ('b, 'ctx) t

  (** [t >>| f] is the same as [t >>= fun x -> return (f x)] but
      slightly more efficient. *)
  val (>>|) : ('a, 'ctx) t -> ('a -> 'b) -> ('b, 'ctx) t
end

(** {1 Forking execution} *)

module Future : sig
  type ('a, 'ctx) fiber

  (** A future represent a promise that will eventually yield a
      value. It is used to represent the result of a fiber running in
      the background. *)
  type ('a, 'ctx) t

  (** Wait for the given future to yield a value. *)
  val wait : ('a, 'ctx) t -> ('a, 'ctx) fiber

  (** Return [Some x] if [t] has already returned. *)
  val peek : ('a, 'ctx) t -> 'a option
end with type ('a, 'ctx) fiber := ('a, 'ctx) t

(** [fork f] creates a sub-fiber and return a [Future.t] to wait its result. *)
val fork : (unit -> ('a, 'ctx) t) -> (('a, 'ctx) Future.t, 'ctx) t

(** [nfork l] is similar to [fork] but creates [n] sub-fibers. *)
val nfork : (unit -> ('a, 'ctx) t) list -> (('a, 'ctx) Future.t list, 'ctx) t

(** [nfork_map l ~f] is the same as [nfork (List.map l ~f:(fun x () ->
    f x))] but more efficient. *)
val nfork_map
  :  'a list
  -> f:('a -> ('b, 'ctx) t)
  -> (('b, 'ctx) Future.t list, 'ctx) t

(** {1 Joining} *)

(** The following combinators are helpers to combine the result of
    several fibers into one. Note that they do not introduce
    parallelism. *)

val both : ('a, 'ctx) t -> ('b, 'ctx) t -> ('a * 'b, 'ctx) t
val all : ('a, 'ctx) t list -> ('a list, 'ctx) t
val all_unit : (unit, 'ctx) t list -> (unit, 'ctx) t

(** {1 Forking + joining} *)

(** The following functions combine forking 2 or more fibers followed
    by joining the results. For every function, we give an equivalent
    implementation using the more basic functions as
    documentation. Note however that these functions are implemented as
    primitives and so are more efficient that the suggested
    implementation. *)

(** For two fibers and wait for their results:

    {[
      let fork_and_join f g =
        fork f >>= fun a ->
        fork g >>= fun b ->
        both (Future.wait a) (Future.wait b)
      ]}
*)
val fork_and_join
  :  (unit -> ('a, 'ctx) t)
  -> (unit -> ('b, 'ctx) t)
  -> ('a * 'b, 'ctx) t

(** Same but assume the first fiber returns [unit]:

    {[
      let fork_and_join_unit f g =
        fork f >>= fun a ->
        fork g >>= fun b ->
        Future.wait a >>> Future.wait b
    ]}
*)
val fork_and_join_unit
  :  (unit -> (unit, 'ctx) t)
  -> (unit -> ('a, 'ctx) t)
  -> ('a, 'ctx) t

(** Map a list in parallel:

    {[
      let parallel_map l ~f =
        nfork_map l ~f >>= fun futures ->
        all (List.map futures ~f:Future.wait)
    ]}
*)
val parallel_map
  : 'a list
  -> f:('a -> ('b, 'ctx) t)
  -> ('b list, 'ctx) t

(** Iter over a list in parallel:

    {[
      let parallel_iter l ~f =
        nfork_map l ~f >>= fun futures ->
        all_unit (List.map futures ~f:Future.wait)
    ]}
*)
val parallel_iter
  :  'a list
  -> f:('a -> (unit, 'ctx) t)
  -> (unit, 'ctx) t

(** {1 Context mamagement} *)

module type Context_types = sig
  type t

  (** Type witness for fibers using [context] *)
  type witness

  (** Proof that [context] and [witness] are equal. *)
  val eq : (t, witness) Type_eq.t
end

module Create_context(Context : Context_types) : sig
  (** Return the current context *)
  val get : (Context.t, Context.witness) t

  (** [set context fiber] sets the context to use in [fiber] *)
  val set : Context.t -> ('b, Context.witness) t -> ('b, _) t
end

(** {1 Error handling} *)

(** [with_error_handler f ~on_error] calls [on_error] for every
    exception raised during the execution of [f]. This include
    exceptions raised when calling [f ()] or during the execution of
    fibers after [f ()] has returned. Exceptions raised by [on_error]
    are passed on to the parent error handler.

    It is guaranteed that after the fiber has returned a value,
    [on_error] will never be called.  *)
val with_error_handler
  :  (unit -> ('a, 'ctx) t)
  -> on_error:(exn -> unit)
  -> ('a, 'ctx) t

(** If [t] completes without raising, then [wait_errors t] is the same
    as [t () >>| fun x -> Ok x]. However, if the execution of [t] is
    aborted by an exception, then [wait_errors t] will complete and
    yield [Error ()].

    Note that [wait_errors] only completes after all sub-fibers have
    completed. For instance, in the following code [wait_errors] will
    only complete after 3s:

    {[
      wait_errors
        (fork_and_join
           (fun () -> sleep 1 >>| fun () -> raise Exit)
           (fun () -> sleep 3))
    ]}

    same for this code:

    {[
      wait_errors
        (fork (fun () -> sleep 3) >>= fun _ -> raise Exit)
    ]}
*)
val wait_errors : ('a, 'ctx) t -> (('a, unit) Result.t, 'ctx) t

(** [fold_errors f ~init ~on_error] calls [on_error] for every
    exception raised during the execution of [f]. This include
    exceptions raised when calling [f ()] or during the execution of
    fibers after [f ()] has returned.

    Exceptions raised by [on_error] are passed on to the parent error
    handler. *)
val fold_errors
  :  (unit -> ('a, 'ctx) t)
  -> init:'b
  -> on_error:(exn -> 'b -> 'b)
  -> (('a, 'b) Result.t, 'ctx) t

(** [collect_errors f] is:

    {[
      fold_errors f
        ~init:[]
        ~on_error:(fun e l -> e :: l)
    ]}
*)
val collect_errors
  :  (unit -> ('a, 'ctx) t)
  -> (('a, exn list) Result.t, 'ctx) t

(** [finalize f ~finally] runs [finally] after [f ()] has terminated,
    whether it fails or succeeds. *)
val finalize
  :  (unit -> ('a, 'ctx) t)
  -> finally:(unit -> (unit, 'ctx) t)
  -> ('a, 'ctx) t

(** {1 Synchronization} *)

(** Write once variables *)
module Ivar : sig
  type ('a, 'ctx) fiber = ('a, 'ctx) t

  (** A ivar is a synchronization variable that can be written only
      once. *)
  type ('a, 'ctx) t

  (** Create a new empty ivar. *)
  val create : unit -> ('a, _) t

  (** Read the contents of the ivar. *)
  val read : ('a, 'ctx) t -> ('a, 'ctx) fiber

  (** Fill the ivar with the following value. This can only be called
      once for a given ivar. *)
  val fill : ('a, 'ctx) t -> 'a -> (unit, 'ctx) fiber

  (** Return [Some x] is [fill t x] has been called previously. *)
  val peek : ('a, _) t -> 'a option
end with type ('a, 'ctx) fiber := ('a, 'ctx) t

module Mutex : sig
  type ('a, 'ctx) fiber = ('a, 'ctx) t
  type 'ctx t
  val create : unit -> _ t
  val with_lock : 'ctx t -> (unit -> ('a, 'ctx) fiber) -> ('a, 'ctx) fiber
end with type ('a, 'ctx) fiber := ('a, 'ctx) t

(** {1 Running fibers} *)

(** Wait for one iteration of the scheduler *)
val yield : unit -> (unit, _) t

(** [run t] runs a fiber until it yield a result. If it becomes clear
    that the execution of the fiber will never terminate, raise
    [Never]. *)
val run : ('a, unit) t -> 'a

exception Never
