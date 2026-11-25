;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :hunchentoot-extensions)

(defclass base-taskmaster (hunchentoot:one-thread-per-connection-taskmaster)
  ((nursery :initarg :nursery
            :initform nil
            :documentation "A nursery that outlives the taskmaster, for tasks that should
outlive the request that starts them. Read this with
TASKMASTER-NURSERY."))
  (:default-initargs
   :max-thread-count 300
   :max-accept-count 320)
  (:documentation "The default taskmaster for base-acceptor."))

(defun taskmaster-nursery (taskmaster)
  "The nursery that TASKMASTER was created with, or NIL if it wasn't
given one. This is also NIL on a taskmaster that was created before the
slot was hot-reloaded."
  (when (slot-boundp taskmaster 'nursery)
    (slot-value taskmaster 'nursery)))
