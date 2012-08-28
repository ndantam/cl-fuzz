;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2012, Georgia Tech Research Corporation
;;;; All rights reserved.
;;;;
;;;; Author(s): Neil T. Dantam <ntd@gatech.edu>
;;;; Georgia Tech Humanoid Robotics Lab
;;;; Under Direction of Prof. Mike Stilman
;;;;
;;;; This file is provided under the following "BSD-style" License:
;;;;
;;;;   Redistribution and use in source and binary forms, with or
;;;;   without modification, are permitted provided that the following
;;;;   conditions are met:
;;;;   * Redistributions of source code must retain the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer.
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;;
;;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;;   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;;   INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;;   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;;   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
;;;;   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;;;   NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;;;   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;;;   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;;;;   OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
;;;;   EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;;;;;;;;;;;;;;;
;;; Package ;;;
;;;;;;;;;;;;;;;
(in-package :cl-user)

(defpackage :cl-fuzz
  (:use :cl)
  (:nicknames :fuzz)
  (:export run-tests
           test-true test-false
           test-eq test-eql test-equal test-equalp test=))

;;;;;;;;;;;;;;
;;; CL-FUZZ ;;
;;;;;;;;;;;;;;
(in-package :cl-fuzz)

(defvar *fuzz-log*)
(defvar *fuzz-input*)
(defvar *fuzz-counts*)
(defvar *fuzz-var*)
(defvar *fuzz-trail*)

(defun test-true (name test-function)
"Call TEST-FUNCTION with no arguments.  If result is true, mark
successful test.  If result is false, print an error message.
NAME: name of this test
TEST-FUNCTION: (lambda ()) => (or nil RESULT)
RESULT: the result of TEST-FUNCTION"
  (let* ((batched (and (boundp '*fuzz-log*)
                       (boundp '*fuzz-input*)
                       (boundp '*fuzz-counts*)))
         (result (if batched
                     ;; catch errors thrown by test
                     (handler-case (funcall test-function)
                       (condition (e) ;error
                         (pprint `(:condition ,name :description ,(write-to-string e) :input ,*fuzz-input*)
                                 *fuzz-log*)
                         (return-from test-true)))
                     ;; throw errors back to our caller (and the debugger, probably)
                     (funcall test-function))))
    (if result
        ;; test passed, increment success count
        (when batched
          (incf (gethash name *fuzz-counts* 0)))
        ;; test failed,
        (if batched
            ;; print failure message to log
            (pprint `(:fail ,name :input ,*fuzz-input*) *fuzz-log*)
            (progn
              (when (boundp '*fuzz-var*)
                (pprint *fuzz-var*))
              (when (boundp '*fuzz-trail*)
                (pprint (reverse *fuzz-trail*)))
              ;; No log, break to debugger
              (break))))
    result))

(defun test-false (name test-function)
  "Call TEST-FUNCTION and test if result is false."
  (test-true name (lambda () (not (funcall test-function)))))

(defun test-test (name test expected-function test-function)
  (test-true name (lambda () (funcall test
                                 (funcall expected-function)
                                 (funcall test-function)))))

(defun test-eq (name expected-function test-function)
  "Call EXPECTED-FUNCTION and TEST-FUNCTION and test if results are #'EQ."
  (test-test name #'eq expected-function test-function))

(defun test-eql (name expected-function test-function)
  "Call EXPECTED-FUNCTION and TEST-FUNCTION and test if results are #'EQL."
  (test-test name #'eql expected-function test-function))

(defun test-equal (name expected-function test-function)
  "Call EXPECTED-FUNCTION and TEST-FUNCTION and test if results are #'EQUAL."
  (test-test name #'equal expected-function test-function))

(defun test-equalp (name expected-function test-function)
  "Call EXPECTED-FUNCTION and TEST-FUNCTION and test if results are #'EQUALP."
  (test-test name #'equal expected-function test-function))

(defun test= (name expected-function test-function)
  "Call EXPECTED-FUNCTION and TEST-FUNCTION and test if results are #'=."
  (test-test name #'= expected-function test-function))

;; TODO: catch assertions in tester and generator functions
(defun run-tests (generator tester &key
                  (formatter #'identity)
                  (log *standard-output*)
                  (count 1))
  "Perform a series of fuzz tests.
GENERATOR: (lambda ()) => fuzz.
TESTER: (lambda (fuzz)) => nil, performs one set of fuzz tests."
  (let ((*fuzz-counts* (make-hash-table))
        (*fuzz-log* log))
    (dotimes (i count)
      (let* ((input (funcall generator))
             (*fuzz-input* (funcall formatter input)))
        (funcall tester input)))
    (print `(:result
             ,(loop for k being the hash-keys of *fuzz-counts*
                 collect (list k (gethash k *fuzz-counts*)))))))

;; each case returns new-structure-1
(defmacro do-operations ((var-lambda-list &optional initial)
                         fuzz
                         &body cases)
  "Perform a series of operations on the fuzz.

Each case should return the value after processing the current
operation, or NIL if the test failed.  INITIAL and the true result of
each of CASES will be DESTRUCTURING-BIND'ed to VAR-LAMBDA-LIST.

FUZZ:  (list (list &rest operation))
CASES: ((destructuring-case-lambda-list) &body body) => result"
  (alexandria:with-gensyms (var fuzz-item block result)
    `(let ((*fuzz-var* nil)
           (*fuzz-trail* nil))
       (block ,block
         (reduce (lambda (,var ,fuzz-item)
                   (push ,fuzz-item *fuzz-trail*)
                   (setq *fuzz-var* ,var)
                   (destructuring-bind ,var-lambda-list ,var
                     (let ((,result
                            (alexandria:destructuring-ecase ,fuzz-item
                              ,@(loop for case in cases
                                   for op = (car case)
                                   for body = (cdr case)
                                   collect
                                     `(,op (test-true ,(car op)
                                                      (lambda () (progn ,@body))))))))
                       (if ,result
                           ,result
                           (return-from ,block nil)))))
                 ,fuzz :initial-value ,initial)))))
