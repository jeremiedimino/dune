(** Non-fatal user errors *)

(** Warnings are user errors that cannot be proper errors for backward
    compatibility reasons *)

(** Emit a user warning. The arguments are interpreted in a similar
    fashion to {!User_error.raise} except that the first paragraph is
    prefixed with "Warning: " rather than "Error: ". *)
val emit
  :  loc:Loc.t
  -> ?hints:User_message.Style.t Pp.t list
  -> User_message.Style.t Pp.t list
  -> _

(** Set the warning reporter. The default one is
    [User_message.prerr]. *)
val set_reporter : (User_message.t -> unit) -> unit
