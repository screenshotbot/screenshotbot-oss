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
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:screenshotbot/github/github-app
                #:transient-github-app
                #:abstract-github-app
                #:fetch-github-app-name
                #:github-app-id
                #:github-app-private-key)
  (:export
   #:github-request
   #:github-create-jwt-token)
  (:local-nicknames (#:plugin #:screenshotbot/github/plugin)))
(in-package :screenshotbot/github/jwt-token)


(defun to-unix-time (time)
  "Convert universal time to New Jersey time"
  (when time (- time (encode-universal-time 0 0 0 1 1 1970 0))))

(defun github-create-jwt-token (&key
                                  github-app
                                  app-id ;; instead of app
                                  private-key ;; instead of app
                                  pem-file)
  (when pem-file
    (setf private-key (uiop:read-file-string pem-file)))

  (cond
    (github-app
       ;; Must only provide the app object or the private
     (assert (not private-key))
     (assert (not app-id))
     (github-create-jwt-token
      :app-id (github-app-id github-app)
      :private-key (github-app-private-key github-app)))
    (t
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
                        ("exp" . ,(+ 300 ts)))))))))

(define-condition github-api-error (error)
  ((code :initarg :code
         :reader github-api-error-code)
   (message :initarg :message
            :reader message)
   (url :initarg :url
        :initform nil
        :reader github-api-error-url
        :documentation "This is mainly used for filtering Sentry crashes"))
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
               :message s
               :url url))
      (json:decode-json-from-string s))))

(defmethod plugin:fetch-app-name ((self plugin:github-plugin))
  "Fetch the app name from the server instead of what's stored locally"
  (fetch-github-app-name
   (make-instance 'transient-github-app
                  :app-id (plugin:app-id self)
                  :private-key (plugin:private-key self))))

(defmethod fetch-github-app-name ((self abstract-github-app))
  (let ((jwt-token (github-create-jwt-token
                    :github-app self)))
    (assoc-value
     (github-request "/app"
                     :jwt-token jwt-token)
     :slug)))
