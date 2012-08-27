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



;;;; EXAMPLE for :CL-FUZZ
;;;;
;;;; To demonstrate usage of :CL-FUZZ, This simple fuzz test compares
;;;; a list stack and a vector stack.


(in-package :cl-user)

;; Generate the random input, aka the fuzz
;; For the stack testing, this is a series of push / pop operations
(defun stack-generator ()
  (loop for i below (random 100)
     collect
       (if (zerop (random 2))
           `(:push ,(random 100))
           '(:pop))))

;; Apply all tests to the fuzz
(defun stack-tester (fuzz)
  ;; reduce across all operations in fuzz
  (reduce (lambda (stacks item)
            (destructuring-bind ((list vector) (op &optional arg))
                (list stacks item)
              ;; dispatch based on the operator
              (ecase op
                (:push (fuzz:test-true :push
                                       (lambda ()
                                         (push arg list)
                                         (vector-push-extend arg vector)
                                         t))
                       (list list vector))
                (:pop (fuzz:test-equal :pop
                                       (lambda () (if (zerop (length vector))
                                                 nil
                                                 (vector-pop vector)))
                                       (lambda () (if list
                                                 (pop list)
                                                 nil)))
                      (list list vector)))))
          fuzz
          :initial-value (list nil
                               (make-array 0 :adjustable t :fill-pointer 0))))

;; Run a fuzz test series
(defun perform-stack-test ()
  (fuzz:perform-tests #'stack-generator
                      #'stack-tester
                      :count 10))
