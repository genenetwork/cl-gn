# cl-gn: Common Lisp GeneNetwork

/Maybe the last word on accelerated web programming/

In this repo we explore Common Lisp for GeneNetwork.

## Hacking

Drop into a development environment with
```
guix shell
```
Start the GeneNetwork web server (listening on port 8080) and the slynk REPL server
(listening on port 4005) using
```
sbcl --load run.lisp
```
If using Emacs, connect to slynk using `M-x sly-connect RET localhost RET 4005
RET`. Happy Hacking!

## genodb genotype database tool

### Building and running tests
This repo also includes the genodb CLI tool to manipulate
GeneNetwork's genotype databases. To build it, run
```
sbcl --load build.lisp
```
To run tests, run
```
sbcl --script tests.lisp
```

### Usage
Convert a plain text GeneNetwork genotype file `BXD.geno` to a genodb
genotype database `bxd` using
```
./genodb import BXD.geno bxd
```
If `bxd` does not exist, it is created. If `bxd` already exists and
contains a matrix, the genotype matrix of `BXD.geno` is stored in
`bxd` as a new version.

Print meta-information and list versions stored in a genotype database
`bxd` using
```
./genodb info bxd
```
