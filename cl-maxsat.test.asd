#|
  This file is a part of cl-maxsat project.
  Copyright (c) 2019 Masataro Asai (guicho2.71828@gmail.com)
|#

(defsystem cl-maxsat.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of cl-maxsat"
  :license "LGPL"
  :depends-on (:cl-maxsat
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval (read-from-string "(5am:run! :cl-maxsat)"))
))
