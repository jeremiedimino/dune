open Import
open Jbuild
open Build.O
open! No_io

type rule =
  { exe: Jbuild.Executables.t
  ; alias_name: string
  ; alias_action: (unit, Action.t) Build.t
  ; alias_stamp: Sexp.t
  ; last_lib: string
  }

module Test_lib = struct
  type t =
    | Ppx_expect
    | Ppx_inline_test

  let of_lib_name = function
    | "ppx_inline_test" -> Some Ppx_inline_test
    | "ppx_expect" -> Some Ppx_expect
    | _ -> None

  module Set = Set.Make(struct
      type t' = t
      type t = t'
      let compare (x : t) (y : t) =
        match x, y with
        | Ppx_expect, Ppx_expect
        | Ppx_inline_test, Ppx_inline_test -> 0
        | Ppx_inline_test, Ppx_expect -> 1
        | Ppx_expect, Ppx_inline_test -> -1
    end)

  let libs_of_set s =
    if Set.mem Ppx_expect s then
      [Lib_dep.direct "ppx_expect.evaluator"]
    else
      []
end

let setup_rules test_libs ~sctx ~dir ~(lib : Jbuild.Library.t) ~scope =
  let name = lib.name ^ "_test_runner" in
  let alias_name = "runtest" in
  let exe_stanza =
    { Jbuild.Executables.names = [name]
    ; link_executables = true
    ; link_flags = Ordered_set_lang.Unexpanded.t (
        Sexp.add_loc ~loc:Loc.none (List [Atom "-linkall"])
      )
    ; modes = Mode.Dict.Set.all
    ; buildable =
        { Buildable.modules = Ordered_set_lang.t (
            Sexp.add_loc ~loc:Loc.none (List [])
          )
        ; libraries = Lib_dep.direct lib.name
                      :: (Test_lib.libs_of_set test_libs)
        ; preprocess = lib.buildable.preprocess
        ; preprocessor_deps = lib.buildable.preprocessor_deps
        ; flags = Ordered_set_lang.Unexpanded.standard
        ; ocamlc_flags = Ordered_set_lang.Unexpanded.standard
        ; ocamlopt_flags = Ordered_set_lang.Unexpanded.standard
        ; js_of_ocaml = Js_of_ocaml.default
        ; gen_dot_merlin = false
        ; lint = Jbuild.Lint.no_lint
        }
    } in
  let exe = Path.relative dir (name ^ ".exe") in
  { exe = exe_stanza
  ; alias_name
  ; alias_stamp = Sexp.List [Atom "ppx-runner"; Atom name]
  ; alias_action =
      (let module A = Action in
       Build.path exe >>>
       Super_context.Deps.interpret sctx ~scope ~dir lib.inline_tests.deps
       >>^ fun _ ->
       A.chdir dir
         (A.run (Ok exe) ["inline-test-runner"; lib.name]))
  ; last_lib = "ppx_inline_test.runner"
  }
;;

let rule sctx ~(lib : Jbuild.Library.t) ~dir ~scope =
  let test_config =
    Jbuild.Preprocess_map.pps lib.buildable.preprocess
    |> List.rev_map ~f:Jbuild.Pp.to_string
    |> List.filter_map ~f:Test_lib.of_lib_name
    |> Test_lib.Set.of_list in
  if Test_lib.Set.is_empty test_config then
    None
  else
    Some (setup_rules test_config ~sctx ~dir ~lib ~scope)
