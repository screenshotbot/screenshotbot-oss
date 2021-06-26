;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/phabricator/diff-promoter
  (:use #:cl
        #:alexandria
        #:../model/company
        #:../model/recorder-run
        #:../promote-api
        #:./commenting-promoter)
  (:export #:phabricator-promoter))

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
     (json:decode-json s))))

(defmethod diff-to-revision ((phab phab-instance) diff-id)
  (parse-integer
   (assoc-value
    (cdar
     (cdar
      (call-conduit phab "differential.querydiffs"
                    `(("ids[0]" . ,diff-id)))))
    :revision-+id+)))

(defmethod create-comment ((phab phab-instance) revision-id message)
  (call-conduit phab "differential.createcomment"
                `(("revision_id" . ,revision-id)
                  ("message" . ,message))))

#+nil
(diff-to-revision *phab* 7821)
#+nil
(create-comment *phab* 3498 "hello world")

(defclass phabricator-promoter (commenting-promoter)
  ((phab :accessor phab)
   (diff-id :accessor diff-id)))

(defmethod maybe-promote ((promoter phabricator-promoter) run)
  (when (phabricator-diff-id run)
    (call-next-method)))

(defmethod maybe-send-tasks ((promoter phabricator-promoter) run)
  (when (phabricator-diff-id run)
    (let ((config (phabricator-config-for-company (recorder-run-company run))))
      (setf (phab promoter) (make-instance 'phab-instance
                                            :url (phabricator-url config)
                                            :api-key (conduit-api-key config)))
      (setf (diff-id promoter) (phabricator-diff-id run)))
    (call-next-method)))

(defmethod add-comment ((promoter phabricator-promoter) comment)
  (let ((phab (phab promoter)))
    (when (and (url phab)
               (api-key phab))
     (let ((revision (diff-to-revision phab (diff-id promoter))))
       (create-comment phab revision comment)))))

(register-promoter 'phabricator-promoter)
