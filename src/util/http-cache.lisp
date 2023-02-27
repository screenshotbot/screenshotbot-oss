;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/http-cache
  (:use #:cl)
  (:export
   #:http-cache
   #:parse-max-age))
(in-package :util/http-cache)

(defclass http-cache ()
  ((lru-cache :initarg :lru-cache
              :reader lru-cache)))

(defun parse-max-age (cache-control)
  (let ((parts (str:split "," cache-control)))
    (or
     (loop for part in parts
           do
              (destructuring-bind (key &optional value) (str:split "=" (str:trim part))
                (when (string-equal "max-age" key)
                  (return
                    ;; Sometimes you'll see things like 30s
                    (parse-integer value :junk-allowed t)))))
     0)))
