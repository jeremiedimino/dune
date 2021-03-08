  $ dune build --root static
  Entering directory 'static'
  deps: ../install/default/lib/foo/foo.cma

TODO30: The following should fail saying that %{read:...} isn't
allowed in this position, because it was indeed not supported by older
versions of Dune. Due to improvements in the core of Dune, this is now
supported. Because the error used to be discovered dynamically as we
were trying to evaluate the dependencies, the new code simply doesn't
detect it. Before we release 3.0, we should add a proper version check
for this feature.

  $ dune build --root dynamic
  Entering directory 'dynamic'
  File "dune", line 1, characters 0-43:
  1 | (alias
  2 |  (name default)
  3 |  (deps %{read:foo}))
  Error: No rule found for foo
  [1]

  $ dune build --root alias-lib-file
  Entering directory 'alias-lib-file'
  deps: ../install/default/lib/foo/theories/a
