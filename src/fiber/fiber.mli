(** Concurrency library *)

module type S = Fiber_intf.S
include S

module type S_with_effects = Fiber_intf.S_with_effects
module type Effect_handler = Fiber_intf.Effect_handler

module With_effects
    (Fiber : S)
    (Handler : Effect_handler with type 'a fiber := 'a Fiber.t) : sig
  include S_with_effects
    with type 'a effect := 'a Handler.effect
    with type local_effect := Handler.local_effect

  (** Wrap a fiber without effects *)
  val wrap : 'a Fiber.t -> 'a t

  (** Handle effects from the given fiber *)
  val handle_effects : Handler.context -> 'a t -> 'a Fiber.t
end

(** {1 Running fibers} *)

(** Wait for one iteration of the scheduler *)
val yield : unit -> unit t

(** [run t] runs a fiber until it yield a result. If it becomes clear
    that the execution of the fiber will never terminate, raise
    [Never]. *)
val run : 'a t -> 'a

exception Never
