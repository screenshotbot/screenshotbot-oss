;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/http-ping
  (:use #:cl)
  (:import-from #:util/request
                #:http-request)
  (:export
   #:http-ping))
(in-package :util/http-ping)

(defun http-ping (url)
  (log:info "Hacky: To exit, C-c and then type (quit)")
  (when (str:emptyp url)
    (error "Need a url argument"))
  (let ((good 0)
        (bad 0)
        (all 0)
        (lock (bt:make-lock)))
    (loop for i below 100000
          do
             (sleep 0.25)
             (incf all)
             (bt:with-lock-held (lock)
               (format t "All: ~a / Good: ~a / Bad: ~a~%" all good bad))
             (finish-output t)
             (bt:make-thread
              (lambda ()
                (handler-case
                    (progn
                      (http-request
                       url
                       :ensure-success t)
                      (bt:with-lock-held (lock)
                        (incf good)))
                  (error (e)
                    (log:error "Got error: ~a for ~a" e url)
                    (bt:with-lock-held (lock)
                      (incf bad)))))))))
