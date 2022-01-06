# cl-gn: Common Lisp GeneNetwork

/Maybe the last word on accelerated web programming/

In this repo we explore the common lisp approach to web programming with parenscript.

Use at your own risk.

## Hacking

Drop into a development environment with
```
guix shell
```
Start GeneNetwork (listening on port 8080) and the slynk REPL server
(listening on port 4005) using
```
sbcl --load run.lisp
```
If using Emacs, connect to slynk using `M-x sly-connect RET localhost RET 4005
RET`. Happy Hacking!
