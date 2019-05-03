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

(defun download-solver (year track name)
  (check-type year fixnum)
  (let* ((dir (namestring (asdf:system-relative-pathname :cl-maxsat (format nil "solvers/~a/~a/" year track))))
         (zip (namestring (merge-pathnames (format nil "~a.zip" name) dir)))
         (runner (namestring (merge-pathnames (format nil "~a/bin/starexec_run_default" name) dir)))
         (bin (namestring (merge-pathnames (format nil "~a/bin/" name) dir)))
         (home (namestring (merge-pathnames (format nil "~a/" name) dir))))
    (ensure-directories-exist zip)
    (unless (probe-file runner)
      (alexandria:unwind-protect-case ()
          (progn
            (handler-case
                (uiop:run-program `("wget" ,(format nil "~a/~a/~a.zip" (cdr (assoc year *base-url*)) track name) "-O" ,zip)
                                  :output t :error t)
              (uiop:subprocess-error ()
                (error 'download-error :year year :track track :name name)))
            (handler-case
                (uiop:run-program `("sh" "-c" ,(format nil "cd ~a; unzip ~a.zip" dir name))
                                  :output t :error t)
              (uiop:subprocess-error ()
                (error 'unzip-error :year year :track track :name name))) 
            (handler-case
                (uiop:run-program `("sh" "-c" ,(format nil "cd ~a; chmod +x starexec_build build/*; MAKEFLAGS=\"-j 4\" ./starexec_build" home))
                                  :output t :error t)
              (uiop:subprocess-error ()
                (error 'build-error :year year :track track :name name))))
        (:abort
         (format *error-output* "~&Aborting, cleaning up~%")
         (uiop:run-program `("rm" "-rv" ,zip ,home)
                           :output t :error t :ignore-error-status t))))
    (unless (probe-file runner)
      (error "Runner script ~a is missing in ~a !" runner bin))
    (handler-case
        (uiop:run-program `("sh" "-c" ,(format nil "chmod +x ~a/*" bin))
                          :output t :error-output t)
      (uiop:subprocess-error ()
        (error 'chmod-error :year year :track track :name name)))
    (values runner bin)))

(defmethod solve ((input pathname) (competition (eql :maxsat-competition)) &rest options &key debug year track name &allow-other-keys)
  (remf options :debug)
  (remf options :solver)
  
  (with-temp (dir :directory t :template "glucose.XXXXXXXX" :debug debug)
    (multiple-value-bind (runner bin) (download-solver year track name)
      (let* ((command (format nil "cd ~a ; bash ~a ~a ~a"
                              bin
                              runner
                              (namestring input)
                              (namestring dir)))
             (result (format nil "~a/result" dir)))
        (format t "~&; ~a" command)
        (multiple-value-match (uiop:run-program command
                                                :output result
                                                :error-output t
                                                :ignore-error-status t)
          ((_ _ 0)
           ;; indeterminite
           (values nil nil nil))
          ((_ _ 10)
           ;; sat
           (parse-wdimacs-output result *instance*))
          ((_ _ 20)
           ;; unsat
           (values nil nil t)))))))
