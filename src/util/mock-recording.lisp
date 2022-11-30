;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :util/mock-recording
    (:use #:cl
          #:alexandria)
  (:import-from #:cl-mock
                #:with-mocks
                #:call-previous
                #:if-called)
  (:export
   #:with-recording))
(in-package :util/mock-recording)

(defun call-with-recording (mocked-function file fn &key record skip-args)
  (ensure-directories-exist file)
  (flet ((remove-skip-args (args)
           (loop for x in args
                 for i from 0 to 10000000
                 if (not (member i skip-args))
                   collect x)))
   (cond
     (record
      (let ((recording))
        (with-mocks ()
          (if-called mocked-function
                     (lambda (&rest args)
                       (log:debug "Bypassing any recorded values")
                       (let ((res
                               (call-previous)))
                         (push (cons (remove-skip-args args) res)
                               recording)
                         res)))
          (let ((res (funcall fn)))
            (cl-store:store (reverse recording) (pathname file))
            res))))
     (t
      (let ((recording (cl-store:restore (pathname file))))
        (with-mocks ()
          (if-called mocked-function
                     (lambda (&rest args)
                       (log:debug "Running recorded value")
                       (let ((next (pop recording)))
                         (unless (equal (car next) (remove-skip-args args))
                           (error "Next args in recording is ~S but got ~S"
                                  (car next) args))
                         (cdr next))))
          (funcall fn)))))))

(defmacro with-recording ((mocked-function file &key record skip-args) &body body)
  `(call-with-recording ',mocked-function ,file
                        (lambda () ,@body)
                        :record ,record
                        :skip-args ,skip-args))
