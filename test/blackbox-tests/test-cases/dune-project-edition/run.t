  $ cat dune-project
  cat: dune-project: No such file or directory
  [1]
  $ mkdir src
  $ touch src/dune
  $ dune build
  Info: appending this line to dune-project: (lang dune 1.0)
  $ cat dune-project
  (lang dune 1.0)
