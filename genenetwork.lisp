(defpackage :genenetwork
  (:use :common-lisp :cl-who :hunchentoot :parenscript)
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

;; parenscript puts strings within single quotes. So, use double quote
;; for HTML attributes.
(setq cl-who:*attribute-quote-char* #\")

(hunchentoot:define-easy-handler (home-ps :uri "/ps") ()
  (with-html-output-to-string (str)
    (:html
     (:head (:title "Parenscript hello world"))
     (:body (:h2 "Parenscript hello world")
            "Please click the link: "
            (:a :href "#"
                :onclick (ps (alert "Hello World!"))
                "Hello World!")))))

(defvar *acceptor*
  (make-instance 'hunchentoot:easy-acceptor
                 :address "localhost"
                 :port 8080))

(defun main ()
  (hunchentoot:start *acceptor*))
