;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/failed-run
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:bknr.indices
                #:hash-index)
  (:local-nicknames (#:channel #:screenshotbot/model/channel))
  (:export
   #:run-failed-on-commit-p))
(in-package :screenshotbot/model/failed-run)

(with-class-validation
 (defclass failed-run (store-object)
   ((channel :initarg :channel
             :reader failed-run-channel
             :index-type hash-index
             :index-reader failed-runs-for-channel)
    (%company :initarg :company
              :reader failed-run-company
              :index-type hash-index
              :index-reader failed-runs-for-company)
    (commit :initarg :commit
            :reader failed-run-commit)
    (%ts :initarg :created-at
         :reader created-at))
   (:default-initargs :created-at (get-universal-time))
   (:metaclass persistent-class)))

(defmethod run-failed-on-commit-p ((channel channel:channel)
                                   (commit string))
  (loop for failed-run in (failed-runs-for-channel channel)
        if (equal (failed-run-commit failed-run)
                  commit)
          return t))
