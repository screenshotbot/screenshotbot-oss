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
  (format t "===========================================~%")
  (format t "~%")
  (format t "  REMINDER: use /test-headers as a convenience endpoint for enterprise
  installs, otherwise it tries to redirect to an OIDC nibble.~%~%")
  (format t "  Hacky: To exit, C-c and then type (quit)~%~%")
  (format t "===========================================~%")
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
                      (let ((content (http-request
                                      url
                                      :ensure-success t)))
                        ;;(log:info "Got content: ~a" content)
                        )
                      (bt:with-lock-held (lock)
                        (incf good)))
                  (error (e)
                    (log:error "Got error: ~a for ~a" e url)
                    (bt:with-lock-held (lock)
                      (incf bad)))))))))
