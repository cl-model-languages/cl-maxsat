;;;; Autogenerated ASD file for system "CL-MAXSAT"
;;;; In order to regenerate it, run update-asdf
;;;; from shell (see https://github.com/phoe-krk/asd-generator)
;;;; For those who do not have update-asdf,
;;;; run `ros install asd-generator` (if you have roswell installed)
;;;; There are also an interface available from lisp:
;;;; (asd-generator:regen &key im-sure)
(defsystem cl-maxsat
 :version "0.1"
 :author "Masataro Asai"
 :mailto "guicho2.71828@gmail.com"
 :license "LGPL"
 :depends-on (:cl-sat
              :trivia
              :alexandria
              :iterate)
 :serial t
 :components ((:module "src"
               :components ((:file "0-package")
                            (:file "2-class")
                            (:file "3-dimacs")
                            (:file "4-competition"))))
 :description "Common Lisp API to MAX-SAT Solvers"
 :in-order-to ((test-op (test-op :cl-maxsat.test))))
