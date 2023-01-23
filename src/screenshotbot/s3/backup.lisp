;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/s3/backup
  (:use #:cl)
  (:import-from #:screenshotbot/s3/core
                #:upload-file))
(in-package :screenshotbot/s3/backup)

(defmethod backup-directory (store
                             directory
                             key)
  #+(and linux lispworks)
  (uiop:with-temporary-file (:pathname p :direction :output
                                       :type "tar.gz")
    (sys:run-shell-command
     (list
      "/bin/tar"
      "cz"
      "-C" (namestring directory)
      "./")
     :output p
     :if-output-exists :supersede)
    (log:info "Uploading archive to ~a on s3" key)
    (upload-file store p key)
    (log:info "Archive upload done")))
