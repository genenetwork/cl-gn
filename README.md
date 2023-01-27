# cl-gn: Common Lisp GeneNetwork

/Maybe the last word on accelerated web programming/

In this repo we explore Common Lisp for GeneNetwork.

## Hacking

Drop into a development environment with
```
guix shell
```
or for a container the more verbose
```
guix shell -C -D -m manifest.scm
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
If the `bxd` database does not exist, it is created as a directory containing .mdb files.
If `bxd` already exists and contains a matrix, the genotype matrix of `BXD.geno` is stored in `bxd` as a new version.

Print meta-information and list versions stored in a genotype database
`bxd` using
```
./genodb info bxd
Path: bxd/
Versions: 1
Keys: 5606

Version 1
  Dimensions: 7321 × 236
```

If you add the same file you'll see the database won't grow, even though it contains two identical copies. That is because genodb deduplicates rows.
