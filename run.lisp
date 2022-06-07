(require :asdf)

;; Load genenetwork.asd from the current directory.
(asdf:load-asd (merge-pathnames #P"genenetwork.asd"
                                (directory-namestring *load-truename*)))

;; Load the genenetwork system and start the web server
(asdf:load-system :genenetwork)
(asdf:load-system :genodb)
(genenetwork:main)

;; Start the REPL server
(asdf:load-system :slynk)
(slynk:create-server :port 4005)
