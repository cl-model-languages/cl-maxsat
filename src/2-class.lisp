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

(defclass maxsat-instance (sat-instance)
  ((soft-clauses :reader soft-clauses :initarg :soft-clauses)))

;; redefines the original SOLVE with LIST
(defmethod solve ((i list) solver &rest args &key (converter #'to-cnf-tseytin) soft-clauses &allow-other-keys)
  (apply #'solve
         (if soft-clauses
             (make-instance 'maxsat-instance :form i :converter converter :soft-clauses soft-clauses)
             (make-instance 'sat-instance    :form i :converter converter))
         solver
         args))

(defmethod initialize-instance ((i maxsat-instance) &rest args &key (converter #'to-cnf-tseytin) soft-clauses &allow-other-keys)
  (remf args :soft-clauses)
  (apply #'call-next-method i
         :soft-clauses
         (mappend (lambda-ematch
                    ((list w form)
                     (ematch (to-cnf form converter)
                       ((or (list* 'and clauses)
                            (<> clauses (list cnf) cnf))
                        (mapcar (lambda (clause)
                                  (list w clause))
                                clauses)))))
                  soft-clauses)
         args))

(defmethod solve ((*instance* maxsat-instance) solver &rest args &key debug &allow-other-keys)
  (with-temp (tmp :template "wcnf.XXXXXXX" :debug debug)
    (with-output-to-file (s tmp :if-exists :supersede)
      (print-wcnf *instance* s))
    (apply #'solve (pathname tmp) solver args)))
