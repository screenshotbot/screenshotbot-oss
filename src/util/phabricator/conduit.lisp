;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :util/phabricator/conduit
  (:use #:cl
        #:alexandria)
  (:export #:phab-instance
           #:call-conduit
           #:url
           #:api-key))

(defclass phab-instance ()
  ((url :initarg :url
        :accessor url)
   (api-key :initarg :api-key
            :accessor api-key)))


(defmethod call-conduit ((phab phab-instance) name params)
  (let* ((params (loop for (k . v) in params
                       collect (cons k (format nil "~a" v))))
         (params `(("api.token" . ,(api-key phab))
                   ,@params)))
    (log:info "using params: ~S" params)
   (with-open-stream (s
                      (dex:post (format nil "~a/api/~a" (url phab) name)
                                :want-stream t
                                :content params))
     (let ((res
             (json:decode-json s)))
       (unless (assoc-value res :result)
         (error "Got conduit error: ~A "(assoc-value res :error--info)))
       res))))
