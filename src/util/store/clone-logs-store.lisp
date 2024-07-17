;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/clone-logs-store
  (:use #:cl)
  (:import-from #:bknr.cluster/store
                #:on-snapshot-save-impl)
  (:import-from #:bknr.cluster/server
                #:data-path)
  (:import-from #:util/copy-file
                #:copy-file-fast))
(in-package :util/store/clone-logs-store)

(defclass clone-logs-store ()
  ()
  (:documentation "A fixture that ensures that all the old log files are kept during the
snapshot phase, useful for backing up purposes."))

(defmethod on-snapshot-save-impl :around ((store clone-logs-store) snapshot-writer done)
  (save-transaction-logs store)
  (call-next-method))

(defun save-transaction-logs (store)
  (let ((dir (ensure-directories-exist
              (path:catdir (data-path store) "old-logs/"))))
    (log:info "Saving transaction logs to ~a" dir)
    (clear-directory dir)
    (let ((files (fad:list-directory (path:catdir (data-path store) "log/"))))
      (loop for file in files
            do
               (let ((dest (make-pathname
                            :name (pathname-name file)
                            :defaults dir)))
                 (log:info "Linking ~a to ~a" file dest)
                 (copy-file-fast
                  file
                  dest))))))

(defun clear-directory (dir)
  (let ((files (fad:list-directory dir)))
    (loop for file in files
          do
             (log:info "Deleting ~a" file)
             (delete-file file))))
