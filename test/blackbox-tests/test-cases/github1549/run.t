Reproduction case for #1549: too many parentheses in installed .dune files

  $ dune build --root backend
  Entering directory 'backend'
  $ dune install --root backend --prefix _install
  Entering directory 'backend'
  Installing _install/lib/dune_inline_tests/META
  Installing _install/lib/dune_inline_tests/dune_inline_tests.dune
  Installing _install/lib/dune_inline_tests/opam
  Installing _install/lib/dune_inline_tests/simple_tests$ext_lib
  Installing _install/lib/dune_inline_tests/simple_tests.cma
  Installing _install/lib/dune_inline_tests/simple_tests.cmi
  Installing _install/lib/dune_inline_tests/simple_tests.cmt
  Installing _install/lib/dune_inline_tests/simple_tests.cmx
  Installing _install/lib/dune_inline_tests/simple_tests.cmxa
  Installing _install/lib/dune_inline_tests/simple_tests.cmxs
  Installing _install/lib/dune_inline_tests/simple_tests.ml

  $ cat backend/_install/lib/dune_inline_tests/dune_inline_tests.dune
  (dune
   2
   ((inline_tests.backend
     1.0
     ((flags :standard)
      (generate_runner
       (run sed "s/(\\*TEST:\\(.*\\)\\*)/let () = \\1;;/" %{impl-files}))))))

  $ env OCAMLPATH=backend/_install/lib dune runtest --root example
  Entering directory 'example'
