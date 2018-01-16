open Import

type rule =
  { exe: Jbuild.Executables.t
  ; alias_name: string
  ; alias_action: (unit, Action.t) Build.t
  ; alias_stamp: Sexp.t
  ; last_lib: string
  }

val rule
  : Super_context.t
  -> lib:Jbuild.Library.t
  -> dir:Path.t
  -> scope:Jbuild.Scope.t
  -> modules:Module.t String_map.t
  -> rule option
