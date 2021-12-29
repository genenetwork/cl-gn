(defpackage :genenetwork
  (:use :common-lisp :hunchentoot)
  (:import-from :legit :current-commit)
  (:import-from :cl-json :encode-json-to-string)
  (:export :main))

(in-package :genenetwork)

(hunchentoot:define-easy-handler (version :uri "/api/version") ()
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (encode-json-to-string
   `((version . ,(asdf:system-version (asdf:find-system :genenetwork)))
     (commit . ,(let ((repository (make-instance 'legit:repository
                                                 :location (sb-posix:getcwd))))
                  (current-commit repository))))))

(hunchentoot:define-easy-handler (home :uri "/") ()
  (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
  "Hello World!")

(defvar *acceptor*
  (make-instance 'hunchentoot:easy-acceptor :port 8080))

(defun main ()
  (hunchentoot:start *acceptor*))
