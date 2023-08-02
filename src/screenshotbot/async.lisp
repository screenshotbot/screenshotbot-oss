;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/async
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:server
                #:*shutdown-hooks*)
  (:import-from #:util/threading
                #:make-thread
                #:max-pool)
  (:import-from #:lparallel.promise
                #:promise)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:sb/future
   #:*magick-pool*))
(in-package :screenshotbot/async)

(defvar *kernel* nil)

(defvar *magick-pool* nil)

(defun reinit-pool ()
  (setf *magick-pool* (make-instance 'max-pool :max (serapeum:count-cpus :default 4))))

(reinit-pool)

#+lispworks
(lw:define-action "When starting image" "Reset magick pool"
  #'reinit-pool)

(def-easy-macro magick-future (&fn fn)
  (let ((promise (lparallel:promise)))
    (prog1
        promise
      (make-thread
       (lambda ()
         (lparallel:fulfill promise (fn)))
       :pool *magick-pool*))))
