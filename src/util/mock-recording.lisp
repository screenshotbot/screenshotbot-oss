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
   #:with-recording
   #:track
   #:recording-mode-p))
(in-package :util/mock-recording)

(defclass function-call ()
  ((function-name :initarg :function-name
             :reader function-name)
   (arguments :initarg :arguments
              :reader arguments)
   (response :initarg :response
             :reader response)
   (other-values :initarg :other-values
                 :initform nil
                 :reader other-values)))

;; Keeps track of the recording of each call
(defvar *recording*)

;; Keeps the configuration
(defvar *runtime-config*)

;; Are we in recording or replay mode? If unbound, we are in neither!
(defvar *recording-mode*)

(defvar *in-recorded-function-p* nil
  "True if we're already inside a recorded function (for instance, nested
tracked functions. In this case we shouldn't record any nested functions.")

(defun track-during-recording (mocked-function &key skip-args)
  (if-called mocked-function
             (lambda (&rest args)
               (log:debug "Bypassing any recorded values")
               (let ((res (multiple-value-list
                           (let ((*in-recorded-function-p* t))
                            (call-previous)))))
                 (let ((function-call (make-instance 'function-call
                                                     :function-name mocked-function
                                                     :arguments (remove-skip-args args skip-args)
                                                     :response (car res)
                                                     :other-values (cdr res))))
                   (unless *in-recorded-function-p*
                     (push function-call *recording*)))
                 (apply #'values res)))))

(defun remove-skip-args (args skip-args)
  (loop for x in args
        for i from 0 to 10000000
        if (not (member i skip-args))
          collect x))

(defun track-during-replay (mocked-function &key skip-args)
  (if-called mocked-function
             (lambda (&rest args)
               (log:debug "Running recorded value")
               (let ((next (pop *recording*)))
                 (unless next
                   (error "No more recorded calls"))
                 (unless (eql (function-name next) mocked-function)
                   (error "Function requested is ~a, but we recorded ~a"
                          (function-name next)
                          mocked-function))
                 (let ((args (remove-skip-args args skip-args)))
                  (unless (equalp (arguments next) args)
                    (error "Next args in recording is ~S but got ~S"
                           (arguments next) args)))
                 (apply #'values
                        (response next)
                        (other-values next))))))

(defun recording-mode-p ()
  *recording-mode*)

(defun track (mocked-function &rest args)
  (cond
    (*recording-mode*
     (apply 'track-during-recording mocked-function args))
    (t
     (apply 'track-during-replay mocked-function args))))

(defun call-with-recording (file fn &key record)
  (ensure-directories-exist file)
  (let ((*recording-mode* record))
    (with-mocks ()
      (cond
        (record
         (let ((*recording* nil))
           (let ((res (funcall fn)))
             (cl-store:store (reverse *recording*) (pathname file))
             res)))
        (t
         (let ((*recording* (cl-store:restore (pathname file))))
           (funcall fn)))))))

(defmacro with-recording ((file &key record) &body body)
  `(call-with-recording ,file
                        (lambda () ,@body)
                        :record ,record))
