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

(defun runner (year track name)
  (print
   (multiple-value-list
    (solve '(and (or a b) (or a !b c)) :maxsat-competition
           :soft-clauses '((3 (or b c)))
           :debug t
           :year year :track track :name name))))


;; (test LMHS
;;   (is-true
;;     (runner 2017 "complete" "LMHS")))
;; 
;; (test MaxHS
;;   (is-true
;;    (runner 2017 "complete" "MaxHS")))
;; 
;; (test Loandra
;;   (is-true
;;    (runner 2017 "complete" "Loandra")))
;;   
;; (test MSUsorting
;;   (is-true
;;     (runner 2017 :complete :msusorting)))

(test Open-WBO
  (is-true
   (runner 2017 :complete :Open-WBO)))

(test maxino
  (is-true
   (runner 2017 :complete :maxino)))


(test QMaxSAT
  (is-true
   (runner 2017 :complete :QMaxSAT)))

(test QMaxSATuc
  (is-true
   (runner 2017 :complete :QMaxSATuc)))

;; (test LMHS-inc
;;   (is-true
;;    (runner 2017 :incomplete :LMHS-inc)))
;; 
;; (test MaxHS-inc
;;   (is-true
;;    (runner 2017 :incomplete :MaxHS-inc)))

(test Open-WBO-LSU
  (is-true
   (runner 2017 :incomplete :Open-WBO-LSU)))

;; (test maxroster
;;   (is-true
;;    (runner 2017 :incomplete :maxroster)))
