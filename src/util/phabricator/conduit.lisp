;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :util/phabricator/conduit
  (:use #:cl
        #:alexandria)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:alexandria
                #:alist-hash-table)
  (:export #:phab-instance
           #:call-conduit
           #:url
           #:api-key
           #:make-phab-instance-from-arcrc))

(defclass phab-instance ()
  ((url :initarg :url
        :accessor url)
   (api-key :initarg :api-key
            :accessor api-key)))

(defun make-phab-instance-from-arcrc (url)
  (let* ((api-url (quri:render-uri (quri:merge-uris
                                    (quri:uri "/api/")
                                    (quri:uri url)))))
   (with-open-file (file "~/.arcrc")
     (let* ((json:*json-identifier-name-to-lisp* #'string)
            (arcrc (json:decode-json file))
            (hosts (assoc-value arcrc :|hosts|))
            (host
              ;; Surely there's a better way to do this.
              (assoc-value hosts (intern api-url "KEYWORD")))
            (token
              (assoc-value host :|token|)))
       (assert token)
       (make-instance 'phab-instance
                       :url url
                       :api-key token)))))


(defmethod call-conduit ((phab phab-instance) name params)
  #+nil(log:debug "initial params: ~s" params)
  (let* ((params (alist-hash-table
                  `(,@params
                    ("__conduit__"
                     .
                     ,(alist-hash-table
                       `(("token" . ,(api-key phab)))))))))
    (log:debug "using params: ~S" params)
   (let ((res
           (http-request
            (format nil "~a/api/~a" (url phab) name)
            :method :post
            :want-string t
            :form-data t
            :parameters `(("params" . ,(json:encode-json-to-string params))
                          ("output" . "json")
                          ("__conduit__" . "1")))))
     (let* ((res
              (json:decode-json-from-string res))
            (error-info
              (assoc-value res :error--info)))
       (when error-info
         (error "Got conduit error: ~A " (str:shorten 500 error-info)))
       res))))
