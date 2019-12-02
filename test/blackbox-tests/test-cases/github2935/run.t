Reproduction case for #2935

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat >dune <<EOF
  > (test
  >  (name test)
  >  (action (run %{test} input))
  >  (deps input))
  > EOF

  $ cat >test.ml <<EOF
  > let ic = open_in Sys.argv.(1) in
  > print_endline (input_line ic)
  > EOF

  $ echo 42 > input
  $ echo 42 > test.expected

  $ dune runtest

  $ cat _build/default/input
  42
