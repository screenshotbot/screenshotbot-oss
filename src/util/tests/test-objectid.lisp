;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util.model.test-object-id
  (:use :cl
   :alexandria
        :fiveam)
  (:import-from :util
   :object-with-oid
                :oid
                :find-by-oid
   ))
(in-package :util.model.test-object-id)

(def-suite* :util.model.test-object-id)

(test simple-creation-and-finding
  (let ((obj (make-instance 'object-with-oid)))
    (is (eql obj
             (find-by-oid (oid obj))))))
