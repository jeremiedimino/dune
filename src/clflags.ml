open Stdune

(*let ocaml_comp_flags = ref ["-g"]*)
let g = ref true
let debug_findlib = ref false
let warnings = ref "-40"
let debug_dep_path = ref false
let external_lib_deps_hint = ref []
let capture_outputs = ref true
let debug_backtraces = ref false
let diff_command = ref None
let auto_promote = ref false
let force = ref false
