;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/health-checks
  (:use #:cl)
  (:import-from #:util/health-check
                #:def-health-check))
(in-package :screenshotbot/sdk/health-checks)

(defvar *crlf* (format nil "~a~a" #\Return #\Linefeed))

(def-health-check upload-large-file-multiple-times ()
  ;; See T1252
  #+(and lispworks darwin) ;; Avoid this health check on Prod
  (let ((content (make-array 333333 :element-type '(unsigned-byte 8) :initial-element 0))
        (hostname
          "screenshotbot.io"))
    (flet ((make-request (&key read-timeout)
             (log:debug "Making request")
             (let ((conn (comm:open-tcp-stream hostname 443
                                               :ssl-ctx drakma::*verifying-context*
                                               :read-timeout read-timeout)))
               (assert conn)
               (write-string "PUT /api/upload-test HTTP/1.1" conn)
               (write-string *crlf* conn)
               (write-string "Content-Length: 333333" conn)
               (Write-string *crlf* conn)
               (format conn "Host: ~a" hostname)
               (write-string *crlf* conn)
               (write-string "Content-type: application/octet-stream" conn)
               (write-string *crlf* conn)
               (write-string *crlf* conn)
               (write-sequence content conn)
               (finish-output conn)

               (read-line conn)
               (close conn))))
      (dotimes (i 20)
        (let ((start-time (get-internal-real-time)))
          (make-request :read-timeout 30)
          (let ((end-time (get-internal-real-time)))
            (when (> (- end-time start-time) 15000)
              (error "Upload took too long, probably a bug"))))))))
