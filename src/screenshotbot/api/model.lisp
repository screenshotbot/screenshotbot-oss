;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/model
            (:use #:cl)
            (:import-from #:json-mop
                          #:json-serializable-class)
            (:local-nicknames (#:a #:alexandria))
            (:export
             #:encode-json
             #:*api-version*
             #:version-number
             #:failed-run
             #:failed-run-channel
             #:failed-run-commit))

(in-package :screenshotbot/api/model)

;; Please update CHANGELOG.md
(defparameter *api-version* 3)

(defclass version ()
  ((version :initarg :version
            :json-key "version"
            :json-type :number
            :reader version-number))
  (:metaclass json-serializable-class))

(defmethod encode-json (object)
  (with-output-to-string (out)
    (json-mop:encode object out)))

(defmethod decode-json (json type)
  (json-mop:json-to-clos json type))

(defclass failed-run ()
  ((id :initarg :id
       :json-type :number
       :json-key "id")
   (channel :initarg :channel
            :json-key "channel"
            :json-type :string
            :reader failed-run-channel)
   (commit :initarg :commit
           :json-key "commit"
           :json-type :string
           :reader failed-run-commit))
  (:metaclass json-serializable-class))
