#|

This file is a part of cl-maxsat project.
Copyright (c) 2019 Masataro Asai (guicho2.71828@gmail.com)
Copyright (c) 2019 IBM Corporation
SPDX-License-Identifier: LGPL-3.0-or-later

CL-MAXSAT is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any
later version.

CL-MAXSAT is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
CL-MAXSAT.  If not, see <http://www.gnu.org/licenses/>.

|#

(in-package :cl-user)
(defpackage :cl-maxsat.test
  (:use :cl
        :cl-maxsat
        :fiveam
        :cl-sat :trivia :alexandria :iterate))
(in-package :cl-maxsat.test)



(def-suite :cl-maxsat)
(in-suite :cl-maxsat)

;; run test with (run! test-name) 

(test print-wcnf
  (fresh-line)
  (let ((*verbosity* 3))
    (finishes (print-wcnf (make-instance 'maxsat-instance :form '(and a b c) :soft-clauses '((5 (or !a !c)) (3 (or !a !b)) (2 (or !b !c))))))))



