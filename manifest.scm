(use-modules (gnu packages lisp)
             (gnu packages lisp-xyz))

(packages->manifest
 (list sbcl sbcl-cl-json sbcl-hunchentoot sbcl-legit sbcl-slynk))
