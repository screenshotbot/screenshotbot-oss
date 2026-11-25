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

(defvar *background-nursery* nil
  "The nursery that BACKGROUND-NURSERY returns, once it has started one.")

(defvar *background-nursery-lock* (bt:make-lock "background-nursery"))

(defun start-background-nursery ()
  "Start a thread that opens a nursery and keeps it open until it's
cancelled, which only happens if one of its tasks fails. Returns the
nursery."
  (let ((lock (bt:make-lock "start-background-nursery"))
        (cv (bt:make-condition-variable))
        (result nil))
    (bt:make-thread
     (lambda ()
       (handler-case
           (nursery:with-nursery (nursery)
             (bt:with-lock-held (lock)
               (setf result nursery)
               (bt:condition-notify cv))
             ;; NURSERY-SLEEP returns early, and exits from the body,
             ;; once the nursery is cancelled.
             (loop (nursery:nursery-sleep 3600)))
         (nursery:nursery-error (e)
           (log:warn "The background nursery was cancelled: ~a" e))))
     :name "background-nursery")
    (bt:with-lock-held (lock)
      (loop until result
            do (bt:condition-wait cv lock)))
    result))

(defun background-nursery ()
  "A nursery that lives as long as the process, for taskmasters that
weren't given one. It's started the first time it's needed. A failing
task cancels a nursery, so if that happens, the next call starts a new
one."
  (bt:with-lock-held (*background-nursery-lock*)
    (let ((nursery *background-nursery*))
      (when (or (null nursery)
                (nursery:cancelledp nursery))
        (setf *background-nursery* (start-background-nursery))))
    *background-nursery*))

(defun taskmaster-nursery (taskmaster)
  "The nursery that TASKMASTER was created with. If it wasn't given one,
or it was created before the slot was hot-reloaded, this is the
BACKGROUND-NURSERY instead."
  (or (when (slot-boundp taskmaster 'nursery)
        (slot-value taskmaster 'nursery))
      (background-nursery)))
