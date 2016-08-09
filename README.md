# jhupllib

This repository contains a standard library of sorts for JHU PL Lab projects (although it is by no means limited to those projects).  The library contains the sort of general-purpose miscellany that we wish to use in multiple projects but which do not have sufficient mass to warrant maintenance as independent libraries.

## Installing

`jhupllib` can be installed via [OPAM](http://opam.ocaml.org).

## Building

This project uses [OASIS](http://oasis.forge.ocamlcore.org/) as a build tool.  After cloning this repository, it should be sufficient to perform the following steps:

  1. Install dependencies.

    `opam install oasis batteries monadlib ocaml-monadic ppx_deriving.std yojson`
  
  2. Create a dynamic OASIS setup file.
  
    `oasis setup -setup-update dynamic`

  3. Configure the project.
  
    `./configure --enable-tests`
  
  4. Build and run tests.
  
    `make test`
