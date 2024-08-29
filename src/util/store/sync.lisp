;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/sync
  (:use #:cl)
  (:import-from #:util/store/encodable
                #:encodable)
  (:import-from #:core/rpc/rpc
                #:call-rpc)
  (:import-from #:util/store/store
                #:generate-sync-test))
(in-package :util/store/sync)

(defclass sync-sha-request (encodable)
  ())

(defmethod call-rpc ((self sync-sha-request))
  (uiop:with-temporary-file (:stream s :pathname p :direction :io)
    (generate-sync-test s)
    (finish-output s)
    (file-position s 0)
    (ironclad:byte-array-to-hex-string
     (ironclad:digest-file :sha256 p))))
