;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/adb-puller
  (:use #:cl)
  (:export
   #:external-data-dir
   #:pull-file
   #:remote-file-exists-p
   #:pull-directory))
(in-package :screenshotbot/sdk/adb-puller)

(defclass adb-puller ()
  ((exec :initarg :exec
         :reader exec
         :documentation "Path to the adb executable")))


(defmethod exec-adb ((self adb-puller)
                     args
                     &key ignore-error-status
                       (output  *standard-output*))
  (uiop:run-program
   (list*
    (namestring (exec self))
    args)
   :output output
   :error-output *standard-output*
   :ignore-error-status ignore-error-status))

(defun android-namestring (remote)
  (let* ((pathname (pathname remote))
         (directory (pathname-directory pathname)))
    (assert (eql :absolute (car directory)))
    (str:concat
     "/"
     (str:join "/" (cdr directory))
     "/"
     (namestring
      (make-pathname
       :directory nil
       :defaults pathname)))))

(defmethod remote-file-exists-p ((self adb-puller)
                                 src)
  (let ((output (exec-adb self
                          (list
                           "shell"
                           (format nil
                                   "ls ~a && echo EXISTS || echo DOES_NOT_EXIST"
                                   (android-namestring src)))
                          :output 'string)))
    (str:containsp "EXISTS" output)))

(defmethod pull-file ((self adb-puller)
                      remote
                      local)
  (let ((output (exec-adb self
                          (list
                           "pull"
                           (android-namestring remote)
                           (namestring local)))))))

(defmethod pull-directory ((self adb-puller)
                           remote
                           local)
  (let ((remote-tar (cl-ppcre:regex-replace-all
                     "/$"
                     (namestring remote)
                     ".zip" )))
    (exec-adb self
              (list
               "shell"
               "zip"
               "-r"
               remote-tar
               (namestring remote))
              :output 'string
              :error-output 'string)
    (uiop:with-temporary-file (:pathname p :type "zip")
      (pull-file self
                 remote-tar
                 p)
      (log:info "Untarring the archive")
      (let ((zipfile (zip:open-zipfile p)))
        (zip:do-zipfile-entries (name entry zipfile)
          (with-open-file (output (ensure-directories-exist (path:catfile local name))
                                  :direction :output
                                  :element-type 'flex:octet)
            (zip:zipfile-entry-contents entry output))))
      (log:info "Cleaning up temporary tar file"))
    (exec-adb self
              (list "shell" "rm" remote-tar))))

(defmethod external-data-dir ((self adb-puller))
  (format nil "~a/"
   (str:trim
    (exec-adb self
              (list "shell" "echo" "$EXTERNAL_STORAGE")
              :output 'string))))
