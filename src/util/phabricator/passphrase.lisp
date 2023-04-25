;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/phabricator/passphrase
  (:use #:cl)
  (:import-from #:json-mop
                #:json-serializable-class)
  (:import-from #:util/phabricator/conduit
                #:call-conduit
                #:make-phab-instance-from-arcrc)
  (:import-from #:alexandria
                #:assoc-value))
(in-package :util/phabricator/passphrase)

(defvar *secret-file* nil
  "A *cache* of the secrets. The source of truth is still Phabricator")

(defclass passphrase ()
  ((name :initarg :name
         :reader passphrase-name)
   (id :initarg :id
       :reader passphrase-id)
   (type :initarg :type)
   (token :initarg :token
          :reader token))
  (:metaclass json-serializable-class))

(defvar *secrets* (fset:empty-map))

(defmacro def-passphrase-token (name &key id)
  `(let ((passphrase (make-instance 'passphrase
                                    :name (string-downcase ',name)
                                    :id ,id
                                    :type :token)))
     (setf *secrets*
           (fset:with *secrets*
                      ',name passphrase))
     (defun ,name ()
       (read-passphrase passphrase))))

(def-passphrase-token stripe-prod-webhook-signing-key :id 14)

;; (stripe-prod-webhook-signing-key)

(defmethod read-passphrase ((self passphrase))
  (let ((phab (make-phab-instance-from-arcrc "https://phabricator.tdrhq.com")))
    (let ((response (call-conduit phab "passphrase.query"
                                  `((:ids . ,(list (passphrase-id self)))
                                    (:need-secrets . t)
                                    (:limit . 1)))))
      (let ((data (cdar (assoc-value (assoc-value response :result) :data))))
        (unless (string-equal (passphrase-name self)
                              (assoc-value data :name))
          (error "Expected key: ~a, but got ~a"
                 (passphrase-name self)
                 (assoc-value data :name)))
        (assoc-value (assoc-value data :material) :token)))))



