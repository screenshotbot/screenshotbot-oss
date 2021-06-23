;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/github/jwt-token
    (:use #:cl
          #:alexandria)
  (:import-from #:../server
                #:*root*)
  (:export #:github-request
           #:github-create-jwt-token))


(defun to-unix-time (time)
  "Convert universal time to New Jersey time"
  (when time (- time (encode-universal-time 0 0 0 1 1 1970 0))))

(defun github-create-jwt-token (&key app-id
                                  private-key
                                  pem-file)
  (when pem-file
    (setf private-key (uiop:read-file-string pem-file)))
  ;; todo: this seems unnecessary. The PEM library currently only
  ;; exposes files. On a multi-tenant server, this might also leak the
  ;; private key.
  (uiop:with-temporary-file (:stream s :pathname pem-file
                             :direction :output :type "pem")
    (write-string private-key s)
    (finish-output s)
    (let* ((key (pem:read-from-file pem-file))
           (ts (to-unix-time (get-universal-time))))
      (assert key)
      (jose:encode :rs256
                   key
                   `(("iss" . ,(format nil "~a" app-id))
                     ("iat" . ,ts)
                     ("exp" . ,(+ 300 ts)))))))

;; (github-create-jwt-token)

(defun github-request (url &key
                             parameters
                             installation-token
                             jwt-token
                             json-parameters
                             method)
  (multiple-value-bind (s res)
      (drakma:http-request
                        (format nil "https://api.github.com~a" url)
                        :want-stream t
                        :method method
                        :accept "application/vnd.github.v3+json"
                        :additional-headers
                        `(("Authorization"
                           .
                           ,(cond
                              (installation-token
                               (format nil "token ~a" installation-token))
                              (jwt-token
                               (format nil "Bearer ~a" jwt-token))
                              (t
                               (error "specify either :jwt-token or :installation-token")))))
                        :parameters (unless json-parameters`(,@parameters))
                        :content (when json-parameters
                                   (json:encode-json-to-string parameters)) )
    (with-open-stream (s s)
      (unless (or (eql res 200) (eql res 201))
        (error "Got bad github error code: ~a (~S)" res
               (ignore-errors (json:decode-json s))))
      (json:decode-json s))))
