(defsystem :genenetwork
  :class :package-inferred-system
  :description "GeneNetwork"
  :version "0.1.0"
  :author "The GeneNetwork team"
  :license "GNU Affero General Public License version 3 or later"
  :depends-on (:genenetwork/genenetwork))

(defsystem :genodb
  :description "GeneNetwork genotype database tool"
  :version "0.1.0"
  :author "The GeneNetwork team"
  :license "GNU General Public License version 3 or later"
  :depends-on (:alexandria
               :cl-fad
               :ironclad
               :listopia
               :lmdb
               :str
               :trivia)
  :components ((:file "genodb"))
  :build-operation "program-op"
  :build-pathname "genodb"
  :entry-point "genodb:main")
