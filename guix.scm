(use-modules (gnu packages lisp)
             (gnu packages lisp-xyz))

(packages->manifest
 (list sbcl sbcl-hunchentoot))
