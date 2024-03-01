;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/fake-clingon
  (:use #:cl)
  (:import-from #:clingon
                #:option-key)
  (:export
   #:make-fake-clingon))
(in-package :util/fake-clingon)

(defclass fake-clingon ()
  ((options :initarg :options
            :reader options)
   (args :initarg :args
         :reader args)))

(defun make-fake-clingon (options &rest args &key &allow-other-keys)
  (make-instance 'fake-clingon
                 :options options
                 :args args))

(defmethod clingon:getopt ((cmd fake-clingon) arg &optional default)
  (loop for opt in (options cmd)
        if (eql (option-key opt) arg)
          do (return)
        finally
           (error "~a is not a valid option" arg))
  (or
   (getf (args cmd) arg)
   default))
