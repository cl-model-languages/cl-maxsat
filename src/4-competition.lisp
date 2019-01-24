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

(defvar *base-url*
  `((2017 . "http://mse17.cs.helsinki.fi/mse17-solver-src/")
    (2018 . "https://maxsat-evaluations.github.io/2018/mse18-solver-src/")))

(define-condition competition-setup-error (error)
  ((year :initarg :year)
   (track :initarg :track)
   (name :initarg :name))
  (:report
   (lambda (c s)
     (print c s))))

(defmethod print-object ((c competition-setup-error) s)
  (print-unreadable-object (c s :type t)
    (with-slots (year track name) c
       (format s "~a ~a ~a" year track name))))

(define-condition download-error (competition-setup-error) ())
(define-condition unzip-error (competition-setup-error) ())
(define-condition build-error (competition-setup-error) ())
(define-condition chmod-error (competition-setup-error) ())
(define-condition no-cplex-error (error) ())

(defun cmd (command &rest format-args)
  "returns a status code, signal errors for non-0 return code"
  (uiop:run-program (apply #'format nil command format-args)
                    :output *standard-output*
                    :error-output *error-output*))

(defun cmd* (command &rest format-args)
  "returns a status code, ignores error status"
  (uiop:run-program (apply #'format nil command format-args)
                    :output *standard-output*
                    :error-output *error-output*
                    :ignore-error-status t))

(defun cmd/s (command &rest format-args)
  "returns a string, signal errors for non-0 return code"
  (uiop:run-program (apply #'format nil command format-args)
                    :output '(:string :stripped t)
                    :error-output *error-output*))

(defun cmd*/s (command &rest format-args)
  "returns a string, ignores error status"
  (uiop:run-program (apply #'format nil command format-args)
                    :output '(:string :stripped t)
                    :error-output *error-output*
                    :ignore-error-status t))

(defun rel (directory)
  (asdf:system-relative-pathname :cl-maxsat directory))

(defmethod solve ((input pathname) (competition (eql :maxsat-competition)) &rest options &key debug year track name &allow-other-keys)
  (remf options :debug)
  (with-temp (dir :directory t :template "maxsat.XXXXXXXX" :debug debug)
    (let ((result (format nil "~a/result" dir)))
      ;; exit value seem to be now ignored in MaxSAT competition
      (download-and-run-solver year track name input dir result)
      (parse-wdimacs-output result *instance*))))


(defgeneric download-and-run-solver (year track name input dir result)
  (:documentation "Returns function"))

(defun detect-cplex ()
  (format t "~&; Detecting cplex ('cplex' binary needs to be in PATH)~&")
  ;; tested for CPLEX 12.8
  ;; /home/masataro/.local/opt/ibm/ILOG/CPLEX_Studio128/cplex/bin/x86-64_linux/cplex
  (handler-case
      (let* ((cplex (cmd/s "readlink -ef $(which cplex)"))
             (studio (truename (merge-pathnames "../../../" (make-pathname :name nil :defaults cplex))))
             (platform (cmd/s "basename $(dirname $(readlink -ef $(which cplex)))")) ; -- e.g. x86-64_linux
             (cplex-dynamic  (cmd/s "dirname $(readlink -ef $(which cplex))"))
             (cplex-static   (merge-pathnames (format nil "cplex/lib/~a/static_pic" platform) studio))
             (cplex-header   (merge-pathnames "cplex/include" studio))
             (concert-static (merge-pathnames (format nil "concert/lib/~a/static_pic" platform) studio))
             (concert-header (merge-pathnames "concert/include" studio)))
        (format t "~&; Found! ~a~&" cplex)
        (values cplex cplex-dynamic cplex-static cplex-header
                concert-static concert-header))
    (uiop:subprocess-error ()
      (error 'no-cplex-error))))

;; LMHS requires CPLEX
#+(or)
(defmethod download-and-run-solver ((year (eql 2017))
                                    (track (eql :complete))
                                    (name  (eql :lmhs))
                                    input dir result)
  (download-and-extract 2017 "complete" "LMHS")
  ;; build
  (let* ((code (namestring (rel (format nil "solvers/~a/~a/~a/code/" year track name)))))
    (unless (prove-file "")
      (handler-case
          (progn
            (simple-style-warning "This requires CPLEX")
            (cmd "cd ~a && make"))
        (uiop:subprocess-error ()
          (error 'build-error :year year :track track :name name))))

    (cmd "" input dir result)))

;; MaxHS requires CPLEX
(defmethod download-and-run-solver ((year (eql 2017))
                                    (track (eql :complete))
                                    (name  (eql :maxhs))
                                    input dir result)
  (download-and-extract 2017 "complete" "MaxHS")
  ;; build
  (multiple-value-bind (cplex bin static header) (detect-cplex)
    (declare (ignorable cplex bin static header))
    ;; build
    (let* ((code (namestring (rel (format nil "solvers/~a/~a/~a/code/" year track name)))))
      (unless nil
        ;; (probe-file (rel (format nil "solvers/~a/~a/~a/code/" year track name)))
        (handler-case
            (progn
              #+linux
              (cmd "cd ~a; LINUX_CPLEXLIBDIR=~a LINUX_CPLEXINCDIR=~a make config" code static header)
              #+darwin
              (cmd "cd ~a; DARWIN_CPLEXLIBDIR=~a DARWIN_CPLEXINCDIR=~a make config" code static header)
              (cmd "cd ~a && make"))
          (uiop:subprocess-error ()
            (error 'build-error :year year :track track :name name))))
      ;; (cmd "" input dir result)
      )))

;; couldnt make it work
#+(or)
(defmethod download-and-run-solver ((year (eql 2017))
                                    (track (eql :complete))
                                    (name  (eql :loandra))
                                    input dir result)
  (download-and-extract 2017 "complete" "Loandra")
  ;; build
  (let* ((code (namestring (rel (format nil "solvers/~a/~a/~a/code/" year track name)))))
    (unless (probe-file "")
      (handler-case
          (progn
            (cmd "cd ~a && make rs"))
        (uiop:subprocess-error ()
          (error 'build-error :year year :track track :name name))))
    (cmd "" input dir result)))


;; MSUSorting builds fine, but the output format is incorrect --- variable assignments are in the "s" line, not "v" line
#+(or)
(defmethod download-and-run-solver ((year (eql 2017))
                                    (track (eql :complete))
                                    (name  (eql :msusorting))
                                    input dir result)
  (let ((track "complete")
        (name "MSUSorting"))
    (download-and-extract 2017 track name)
    ;; build
    (unless (probe-file (rel (format nil "solvers/~a/~a/~a/code/sortingmaxsat" year track name)))
      (handler-case
          (progn
            (cmd "make -C ~a"    (rel (format nil "solvers/~a/~a/~a/code/depend"         year track name)))
            (cmd "rm ~a || true" (rel (format nil "solvers/~a/~a/~a/code/CMakeCache.txt" year track name)))
            (cmd "cd ~a ; cmake ."      (rel (format nil "solvers/~a/~a/~a/code/"               year track name)))
            (cmd "make -C ~a"    (rel (format nil "solvers/~a/~a/~a/code/"               year track name))))
        (uiop:subprocess-error ()
          (error 'build-error :year year :track track :name name))))
    (cmd* "~a -f ~a > ~a" (rel (format nil "solvers/~a/~a/~a/code/sortingmaxsat" year track name)) input result)))

(defmethod download-and-run-solver ((year (eql 2017)) (track (eql :complete)) (name  (eql :open-wbo))
                                    input dir result)
  (let ((track "complete")
        (name "Open-WBO"))
    (download-and-extract 2017 track name)
    ;; build
    (unless (probe-file (rel (format nil "solvers/~a/~a/~a/code/open-wbo" year track name)))
      (handler-case
          (progn
            (cmd "sed -i 's/-Wall -Wno-parentheses//g' ~a"
                 (rel (format nil "solvers/~a/~a/~a/code/Makefile"         year track name)))
            (cmd "cd ~a ; make "    (rel (format nil "solvers/~a/~a/~a/code/"         year track name))))
        (uiop:subprocess-error ()
          (error 'build-error :year year :track track :name name))))
    (cmd* "~a ~a > ~a" (rel (format nil "solvers/~a/~a/~a/code/open-wbo" year track name)) input result)))

(defmethod download-and-run-solver ((year (eql 2017)) (track (eql :complete)) (name  (eql :maxino))
                                    input dir result)
  (let ((track "complete")
        (name "maxino"))
    (download-and-extract 2017 track name)
    ;; build
    (unless (probe-file (rel (format nil "solvers/~a/~a/~a/code/build/release/maxino" year track name)))
      (handler-case
          (progn
            (cmd "make -C ~a all static lib"    (rel (format nil "solvers/~a/~a/~a/code/" year track name))))
        (uiop:subprocess-error ()
          (error 'build-error :year year :track track :name name))))
    (cmd* "~a ~a > ~a" (rel (format nil "solvers/~a/~a/~a/code/build/release/maxino" year track name)) input result)))

(defmethod download-and-run-solver ((year (eql 2017)) (track (eql :complete)) (name  (eql :qmaxsat))
                                    input dir result)
  (let ((track "complete")
        (name "QMaxSAT"))
    (download-and-extract 2017 track name)
    ;; build
    (unless (probe-file (rel (format nil "solvers/~a/~a/~a/code/qmaxsat1703_g3" year track name)))
      (handler-case
          (progn
            (cmd "cd ~a ; make"    (rel (format nil "solvers/~a/~a/~a/code/" year track name))))
        (uiop:subprocess-error ()
          (error 'build-error :year year :track track :name name))))
    (cmd* "~a ~a > ~a" (rel (format nil "solvers/~a/~a/~a/code/qmaxsat1703_g3" year track name)) input result)))

(defmethod download-and-run-solver ((year (eql 2017)) (track (eql :complete)) (name  (eql :qmaxsatuc))
                                    input dir result)
  (let ((track "complete")
        (name "QMaxSATuc"))
    (download-and-extract 2017 track name)
    ;; build
    (unless (probe-file (rel (format nil "solvers/~a/~a/~a/code/qmaxsat1706UC_g3" year track name)))
      (handler-case
          (progn
            (cmd "cd ~a ; make"    (rel (format nil "solvers/~a/~a/~a/code/" year track name))))
        (uiop:subprocess-error ()
          (error 'build-error :year year :track track :name name))))
    (cmd* "~a ~a > ~a" (rel (format nil "solvers/~a/~a/~a/code/qmaxsat1706UC_g3" year track name)) input result)))


(defmethod download-and-run-solver ((year (eql 2017)) (track (eql :incomplete)) (name  (eql :open-wbo-LSU))
                                    input dir result)
  (let ((track "incomplete")
        (name "Open-WBO-LSU"))
    (download-and-extract 2017 track name)
    ;; build
    (unless (probe-file (rel (format nil "solvers/~a/~a/~a/code/open-wbo" year track name)))
      (handler-case
          (progn
            (cmd "sed -i 's/-Wall -Wno-parentheses//g' ~a"
                 (rel (format nil "solvers/~a/~a/~a/code/Makefile"         year track name)))
            (cmd "cd ~a ; make "    (rel (format nil "solvers/~a/~a/~a/code/"         year track name))))
        (uiop:subprocess-error ()
          (error 'build-error :year year :track track :name name))))
    (cmd* "~a ~a > ~a" (rel (format nil "solvers/~a/~a/~a/code/open-wbo" year track name)) input result)))

;; somehow the results contain undeclared variables; for a problem with 6 variables, it returns a solution with 10 variables
#+(or)
(defmethod download-and-run-solver ((year (eql 2017)) (track (eql :incomplete)) (name  (eql :maxroster))
                                    input dir result)
  (let ((track "incomplete")
        (name "maxroster"))
    (download-and-extract 2017 track name)
    ;; build
    (unless (probe-file (rel (format nil "solvers/~a/~a/~a/bin/maxroster" year track name)))
      (handler-case
          (progn
            (cmd "cd ~a ; make "    (rel (format nil "solvers/~a/~a/~a/code/linux/"         year track name))))
        (uiop:subprocess-error ()
          (error 'build-error :year year :track track :name name))))
    (cmd* "~a ~a > ~a" (rel (format nil "solvers/~a/~a/~a/bin/maxroster" year track name)) input result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun download-and-extract (year track name)
  (check-type year fixnum)
  (let* ((dir (namestring (asdf:system-relative-pathname :cl-maxsat (format nil "solvers/~a/~a/" year track))))
         (zip (namestring (merge-pathnames (format nil "~a.zip" name) dir)))
         (home (namestring (merge-pathnames (format nil "~a/" name) dir))))
    (ensure-directories-exist zip)
    (unless (probe-file zip)
      (alexandria:unwind-protect-case ()
          (progn
            (handler-case
                (cmd "wget ~a/~a/~a.zip -O ~a" (cdr (assoc year *base-url*)) track name zip)
              (uiop:subprocess-error ()
                (error 'download-error :year year :track track :name name)))
            (handler-case
                (cmd "cd ~a; unzip ~a.zip" dir name)
              (uiop:subprocess-error ()
                (error 'unzip-error :year year :track track :name name))))
        (:abort
         (format *error-output* "~&Aborting, cleaning up~%")
         (cmd "rm -rv ~a ~a" zip home))))))


