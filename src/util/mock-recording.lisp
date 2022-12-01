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

(defclass function-call ()
  ((function-name :initarg :function-name
             :reader function-name)
   (arguments :initarg :arguments
              :reader arguments)
   (response :initarg :response
             :reader response)))

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
                         (let ((function-call (make-instance 'function-call
                                                             :function-name mocked-function
                                                             :arguments (remove-skip-args args)
                                                             :response res)))
                           (push function-call recording))
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
                         (unless (equal (arguments next) (remove-skip-args args))
                           (error "Next args in recording is ~S but got ~S"
                                  (car next) args))
                         (response next))))
          (funcall fn)))))))

(defmacro with-recording ((mocked-function file &key record skip-args) &body body)
  `(call-with-recording ',mocked-function ,file
                        (lambda () ,@body)
                        :record ,record
                        :skip-args ,skip-args))
