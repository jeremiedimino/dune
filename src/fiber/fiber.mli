(** Concurrency library *)

module type S = Fiber_intf.S
include S

(** {1 Running fibers} *)

(** Wait for one iteration of the scheduler *)
val yield : unit -> unit t

(** [run t] runs a fiber until it yield a result. If it becomes clear
    that the execution of the fiber will never terminate, raise
    [Never]. *)
val run : 'a t -> 'a

exception Never
