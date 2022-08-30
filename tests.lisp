(require :asdf)

;; Load genenetwork.asd from the current directory.
(asdf:load-asd (merge-pathnames #P"genenetwork.asd"
                                (directory-namestring *load-truename*)))

(asdf:load-system :genodb/tests)
