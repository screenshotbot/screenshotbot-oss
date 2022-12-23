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
  (:import-from #:screenshotbot/sdk/bundle
                #:streamed-image
                #:abstract-image)
  (:import-from #:screenshotbot/sdk/android
                #:read-image
                #:image-bundle)
  (:import-from #:screenshotbot/sdk/sdk
                #:single-directory-run)
  (:local-nicknames (#:sdk #:screenshotbot/sdk/sdk)
                    (#:flags #:screenshotbot/sdk/flags)
                    (#:adb-puller #:screenshotbot/sdk/adb-puller))
  (:export
   #:main))
(in-package :screenshotbot/sdk/gradle)

(defclass adb-image (abstract-image)
  ((pathname :initarg :pathname
             :accessor remote-pathname
             :documentation "The remote pathname of the device"))
  (:documentation "An image that is by default pulled from the remote device via adb."))

(defclass facebook-bundle (image-bundle)
  ((adb :initarg :adb
        :reader adb)
   (remote-metadata-pathname :initarg :remote-metadata-pathname
                             :reader remote-metadata-pathname)
   (tests-run-id :initarg :tests-run-id
                 :reader tests-run-id
                 :documentation "There's a feature in the library, where they try to store images in
the same directory across multiple instrumentation test runs. In that
format, all the files are in the $DIR/$test-run-id/name.png.

This doesn't happen in Firebase, probably because
SCREENSHOT_TESTS_RUN_ID is not set. So we should probably try handling
the situation that this is empty.")
   (tmpdir :initarg :tmpdir
           :reader tmpdir)))

(defmethod read-image ((bundle facebook-bundle)
                       name)
  (let* ((name (make-pathname :type "png"
                              :name name))
         (cache-file (path:catfile (tmpdir bundle) name)))
    (flet ((read-cache ()
             (imago:read-image cache-file)))
      (cond
        ((path:-e cache-file)
         (read-cache))
        (t
         (adb-puller:pull-file
          (adb bundle)
          (path:catfile (remote-metadata-pathname bundle)
                        (if (tests-run-id bundle)
                            (format nil "~a/" (tests-run-id bundle))
                            "")
                        name)
          cache-file)
         (read-cache))))))

(defmacro def-ext-fun (name args &body body)
  `(progn
     (defun ,name ,args
       ,@body
       1)
     (lw:deliver-keep-symbol-names ',name)
     (lw:deliver-keep-symbols ',name)))

(def-ext-fun record-facebook-task (adb package channel)
  (let ((adb (make-instance 'adb-puller :exec adb)))
    (let* ((sdcard (external-data-dir adb))
           (metadata (path:catfile
                      sdcard "screenshots/"
                      (format nil "~a/" package)
                      "screenshots-default/metadata.json"))
           (test-run-id-remote (path:catfile metadata "tests_run_id")))
      (uiop:with-temporary-file (:prefix "metadata" :type "xml"
                                 :pathname p)
        (pull-file
         adb
         metadata
         p)
        (log:info "Retrieved metadata.json")
        (uiop:with-temporary-file (:pathname test-run-id)
          (pull-file adb test-run-id-remote test-run-id)
          (tmpdir:with-tmpdir (tmpdir)
            (let ((bundle (make-instance 'facebook-bundle
                                         :metadata p
                                         :remote-metadata-pathname metadata
                                         :tests-run-id (str:trim (uiop:read-file-string test-run-id))
                                         :adb adb
                                         :tmpdir tmpdir)))
              (sdk:parse-org-defaults)
              (let ((flags:*main-branch* "master"))
                (single-directory-run bundle :channel channel)))))))))

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
