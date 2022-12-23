;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/gradle
  (:use #:cl)
  (:import-from #:screenshotbot/sdk/adb-puller
                #:pull-file
                #:external-data-dir
                #:adb-puller)
  (:export
   #:main))
(in-package :screenshotbot/sdk/gradle)

(defmacro def-ext-fun (name args &body body)
  `(progn
     (defun ,name ,args
       ,@body
       1)
     (lw:deliver-keep-symbol-names ',name)
     (lw:deliver-keep-symbols ',name)))

(def-ext-fun record-facebook-task (adb package)
  (let ((adb (make-instance 'adb-puller :exec adb)))
    (let* ((sdcard (external-data-dir adb))
           (metadata (path:catfile
                      sdcard "screenshots/"
                      (format nil "~a/" package)
                      "screenshots-default/metadata.json")))
      (uiop:with-temporary-file (:prefix "metadata" :type "xml"
                                 :pathname p)
        (pull-file
         adb
         metadata
         p)
        (log:info "Retrieved metadata.json")))))

(defun read-sym (a)
  (find-symbol
   (str:upcase a)
   #.(find-package :screenshotbot/sdk/gradle)))

(defun main ()
  (uiop:setup-command-line-arguments)
  (let ((args (cdr
               #+lispworks system:*line-arguments-list*
               #-lispworks (uiop:command-line-arguments))))
    (format t "Got args: ~S~%" args)
    (apply (read-sym (car args))
           (cdr args)))
  (uiop:quit 0))
