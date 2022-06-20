;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/bknr-slynk
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:slynk
                #:read-from-minibuffer-in-emacs
                #:do-checklist
                #:all-slots-for-inspector)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/bknr-slynk)

#+broken
(defun query-and-set-slot (class object slot)
  (let* ((slot-name (slynk-mop:slot-definition-name slot))
         (value-string (read-from-minibuffer-in-emacs
                        (format nil "Set slot ~S to (evaluated) : "
                                slot-name))))
    (when (and value-string (not (string= value-string "")))
      (with-simple-restart (abort "Abort setting slot ~S" slot-name)
        (bknr.datastore:with-transaction ()
         (setf (slynk-mop:slot-value-using-class class object slot)
               (eval (read-from-string value-string))))))))

#+broken
(defmethod all-slots-for-inspector ((object store-object))
  (let* ((ret (call-next-method))
         (class (class-of object))
         (effective-slots (slynk-mop:class-slots class)))
    (append
     ret
     `((:action "[set bknr value]"
                ,(lambda ()
                   (do-checklist (idx checklist)
                     (query-and-set-slot class object
                                         (nth idx effective-slots)))))))))
