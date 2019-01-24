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
(in-package :cl-maxsat)

;; http://www.maxhs.org/docs/wdimacs.html
;; 
;; * Unweighted Max-SAT with no hard clauses input format: same format as DIMACS
;; but we don't use this format. Instead, use the WDIMACS with equal weights.
;;
;; * Weighted Max-SAT input format
;; * Weighted Partial Max-SAT input format
;; we use the same function for these two cases

;; c
;; c comments Partial Max-SAT
;; c wcnf num_var num_clauses top
;; c top: threashold for the hard clauses. Many solvers require this to be larger than the sum of optional clauses + 1
;; c
;; p wcnf 4 5 15
;; 15 1 -2 4 0
;; 15 -1 -2 3 0
;; 2 -2 -4 0
;; 5 -3 2 0
;; 3 1 3 0
;; ^
;; weights, float or int larger than 0, but many solvers only support ints.
;; 

;; *verbosity* is from cl-sat and is reexported

(defun print-wcnf (instance &optional (stream *standard-output*) (*verbosity* *verbosity*))
  (ematch instance
    ((maxsat-instance (cl-sat::cnf cnf) soft-clauses variables)
     (when (<= 1 *verbosity*)
       (pprint-logical-block (stream nil :per-line-prefix "c ")
         (when (<= 2 *verbosity*)
           (format stream "~&Hard clauses:")
           (format stream "~&~a" cnf)
           (format stream "~&Soft clauses:")
           (format stream "~&~a" soft-clauses))
         (iter (for i from 1)
               (for v in-vector variables)
               (format stream "~&Variable ~a : ~a" i v))))

     (match cnf
       ((or (list* 'and clauses)
            (<> clauses (list cnf)))

        (let ((top (+ 1 (reduce #'+ soft-clauses :key #'first))))
        
          (format stream "~&p wcnf ~A ~A ~A"
                  (length variables)
                  (+ (length clauses)
                     (length soft-clauses))
                  top)
        
          (iter (for c in clauses)
                (ematch c
                  ((or (list* 'or terms)
                       (<> terms (list c)))
                   (when (<= 3 *verbosity*)
                     (format stream "~&c Hard clause: ~a" c))
                   (format stream "~&~a ~{~a ~}0"
                           top
                           (iter (for term in terms)
                                 (collect
                                     (ematch term
                                       ((list 'not atom)
                                        (- (1+ (position atom variables))))
                                       (atom
                                        (1+ (position atom variables))))))))))

          (iter (for c in soft-clauses)
                (ematch c
                  ((list (and w (number))
                         (and disjunction
                              (or (list* 'or terms)
                                  (<> terms (list atom) atom))))
                   (when (<= 3 *verbosity*)
                     (format stream "~&c Soft clause: w=~a, ~a" w disjunction))
                   (format stream "~&~a ~{~a ~}0"
                           w
                           (iter (for term in terms)
                                 (collect
                                     (ematch term
                                       ((list 'not atom)
                                        (- (1+ (position atom variables))))
                                       (atom
                                        (1+ (position atom variables)))))))))))
        (fresh-line stream))))))

(defun parse-wdimacs-output (file instance)
  (iter (for line in-file file using #'read-line)

        (with cost = nil)
        (with sure = nil)
        (with satisfiable = nil)
        (with assignments = (make-array (length (variables instance))
                                        :element-type '(integer 0 2)
                                        :initial-element 2))
        
        (match line
          ((string* #\c _)
           ;; do nothing
           )
          ((string* #\o _)
           ;; optimal solution cost
           (with-input-from-string (s (subseq line 2))
             (setf cost (read s))))
          ((string* #\v _)
           (with-input-from-string (s (subseq line 2))
             (iter (for v in-stream s)
                   (until (zerop v))
                   (setf (aref assignments (1- (abs v)))
                         (if (plusp v) 1 0)))))
          ("s OPTIMUM FOUND"
           (setf sure t satisfiable t))
          ("s UNSATISFIABLE"
           (setf sure t satisfiable nil))
          ("s UNKNOWN"
           (setf sure nil satisfiable nil))
          (_
           (simple-style-warning "found a garbage line in the output: ~a" line)))

        (finally
         (iter (for a in-vector assignments with-index i)
               (for v = (aref (variables instance) i))
               (case a
                 (1 (when (not (eq (find-package :cl-maxsat.aux-variables)
                                   (symbol-package v)))
                      (collect v into trues)))
                 (2 (when (not (eq (find-package :cl-maxsat.aux-variables)
                                   (symbol-package v)))
                      (collect v into dont-care))))
               (finally
                (return-from parse-wdimacs-output
                  (values trues satisfiable sure dont-care cost)))))))

