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
                #:assoc-value)
  (:export
   #:*secret-file*
   #:reload-secrets))
(in-package :util/phabricator/passphrase)

(defvar *secret-file* nil
  "A *cache* of the secrets. The source of truth is still Phabricator")

(defun reload-secrets ()
  (load-passphrases *secret-file*))

(defclass passphrase ()
  ((name :initarg :name
         :reader passphrase-name)
   (id :initarg :id
       :json-key "id"
       :json-type :number
       :reader passphrase-id)
   (type :initarg :type)
   (token :initarg :token
          :json-key "token"
          :json-type :string
          :initform nil
          :accessor token))
  (:metaclass json-serializable-class))

(defvar *secrets* nil)

(defmacro def-passphrase-token (name &key id)
  `(unless (assoc-value *secrets* ',name)
     (let ((passphrase (make-instance 'passphrase
                                      :name (string-downcase ',name)
                                      :id ,id
                                      :type :token)))
       (setf (assoc-value *secrets* ',name)
             passphrase)
       (defun ,name ()
         (read-passphrase passphrase)))))

(def-passphrase-token stripe-prod-webhook-signing-key :id 14)

;; (stripe-prod-webhook-signing-key)

(defun save-passphrases (output)
  (let ((secrets (mapcar #'cdr *secrets*)))
    (mapc #'read-passphrase secrets)
    (with-open-file (output output :if-exists :supersede
                                   :direction :output)
      (yason:encode secrets output))))

(defun load-passphrases (input)
  (with-open-file (input input :direction :input)
    (let ((secrets (json:decode-json input)))
      (flet ((%find (id)
               (loop for secret in secrets
                     if (eql (assoc-value secret :id)
                             id)
                       return secret)))
        (loop for (nil . secret) in *secrets*
              do (setf (token secret)
                       (assoc-value (%find (passphrase-id secret)) :token)))))))

(defmethod read-passphrase ((self passphrase))
  (util/misc:or-setf
   (token self)
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
         (assoc-value (assoc-value data :material) :token))))))
