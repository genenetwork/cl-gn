(defpackage :genenetwork
  (:use :common-lisp :hunchentoot)
  (:export :main))

(in-package :genenetwork)

(hunchentoot:define-easy-handler (home :uri "/") ()
  (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
  "Hello World!")

(defvar *acceptor*
  (make-instance 'hunchentoot:easy-acceptor :port 8080))

(defun main ()
  (hunchentoot:start *acceptor*))
