;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/sharing
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:util/object-id
                #:object-with-oid)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:bknr.indices
                #:hash-index)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/model/sharing)

(with-class-validation
 (defclass share (object-with-oid)
   ((object :initarg :object
            :reader share-object
            :index-type hash-index
            :index-reader shares-for-object)
    (creator :initarg :creator
             :reader share-creator)
    (%company :initarg :company
              :reader company)
    (expiry-date :initarg :expiry-date
                 :reader expiry-date
                 :type (or null string)
                 :documentation "expiration date in yyyy-mm-dd format")
    (revoked-p :initform nil
             :reader share-revoked-p))
   (:metaclass persistent-class)))

(defmethod share-expired-p ((share share))
  (or
   (share-revoked-p share)
   (unless (str:emptyp (expiry-date share))
    (local-time:timestamp>=
     (local-time:now)
     (local-time:parse-timestring (expiry-date share))))))
