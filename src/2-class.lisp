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

