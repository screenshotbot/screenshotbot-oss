;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop/package:define-package :screenshotbot/github/jwt-token
    (:use #:cl #:alexandria)
  (:import-from #:screenshotbot/server
                #:*root*)
  (:import-from #:util/request
                #:http-request)
  (:export
   #:github-request
   #:github-create-jwt-token))
(in-package :screenshotbot/github/jwt-token)


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

(define-condition github-api-error (error)
  ((code :initarg :code
         :reader github-api-error-code)
   (message :initarg :message
            :reader message))
  (:report (lambda (e output)
             (with-slots (code message) e
               (format output "Got bad github error code: ~a (~S)"
                       code message)))))

(auto-restart:with-auto-restart (:retries 3)
  (defun github-request (url
                         &key parameters installation-token
                           jwt-token
                           (json-parameters nil) ;; boolean
                           (method :get))
    (when (and parameters (eql method :get))
      (error "parameters not supported with :GET"))
    (multiple-value-bind (s res)
        (http-request
         (format nil "https://api.github.com~a" url)
         :method method
         :want-string t
         :additional-headers
         `(("Accept" . "application/vnd.github.v3+json")
           ("Authorization"
            .
            ,(cond
               (installation-token
                (format nil "token ~a" installation-token))
               (jwt-token
                (format nil "Bearer ~a" jwt-token))
               (t
                (error "specify either :jwt-token or :installation-token")))))
         :content (if json-parameters
                      (json:encode-json-to-string parameters)
                      parameters))
      (unless (or (eql res 200) (eql res 201))
        (error 'github-api-error
               :code res
               :message s))
      (json:decode-json-from-string s))))
