  $ $JBUILDER runtest -j1 --root .
      ocamlopt .ppx/ppx_inline_test/ppx.exe
           ppx mylib.pp.ml
      ocamldep mylib.depends.ocamldep-output
        ocamlc mylib.{cmi,cmo,cmt}
      ocamlopt mylib.{cmx,o}
      ocamlopt mylib.{a,cmxa}
      ocamlopt mylib_test_runner.exe
  mylib_test_runner alias runtest (exit 1)
  (cd _build/default && ./mylib_test_runner.exe inline-test-runner mylib)
  You are doing something unexpected with the tests. No tests have 
  been run. You should use the inline_tests_runner script to run 
  tests.
  [1]
