;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/assets
  (:use #:cl
        #:alexandria
        #:screenshotbot/artifacts)
  (:import-from #:screenshotbot/server
                #:staging-p
                #:defhandler)
  (:export
   #:define-css
   #:define-js))
(in-package :screenshotbot/assets)

(named-readtables:in-readtable :interpol-syntax)

(defhandler (static-recorder-master :uri "/release/recorder-master.jar") ()
  (hunchentoot:handle-static-file #P "/home/arnold/builds/silkwrmsdk/binary/build/libs/binary-0.1.0-all.jar"))


(flet ((output-file ()  (asdf:output-file 'asdf:compile-op
                                          (asdf:find-component :screenshotbot.sdk/deliver "deliver-sdk"))) )
 (defhandler (static-recorder-linux :uri "/release/recorder-linux.sh") ()
   (hunchentoot:handle-static-file
    (output-file)))

  (defhandler (nil :uri "/recorder-linux.sh") ()
    (let ((util.cdn:*cdn-cache-key* (format nil "~d" (file-write-date (output-file)))))
     (hunchentoot:redirect (util.cdn:make-cdn "/release/recorder-linux.sh"))))
)

(defhandler (nil :uri "/recorder-master.jar") ()
  (hex:safe-redirect "/release/recorder-master.jar"))

(defmacro define-css (uri asdf-target)
  (let ((map-uri (format nil "~a.map" uri)))
   `(progn
      (defhandler (nil :uri ,uri :html nil) ()
        (setf (hunchentoot:content-type*)  "text/css; charset=utf-8")
        (handle-asdf-output 'asdf:compile-op  ,asdf-target))
      (defhandler (nil :uri ,map-uri :html nil) ()
        (handle-asdf-output 'asdf:compile-op ,asdf-target 1)))))


(define-css "/assets/css/default.css" :screenshotbot.css-assets)

(defmacro define-platform-asset (name)
  `(progn
     (defhandler (nil :uri ,(format nil "/~a.sh" name) :html nil) (no-cdn)
       (setf (hunchentoot:content-type*) "application/x-sh")
       (let ((darwin-link (artifact-link ,(format nil "~a-darwin" name) :cdn (not no-cdn)))
             (linux-link (artifact-link ,(format nil "~a-linux" name) :cdn (not no-cdn))))
       #?"#!/bin/sh
set -e
set -x

type=`uname`

INSTALLER=screenshotbot-installer.sh

if [ $type = \"Linux\" ] ; then
  curl --progress-bar ${linux-link} --output $INSTALLER
elif [ $type = \"Darwin\" ] ; then
  curl --progress-bar  ${darwin-link} --output $INSTALLER
else
  echo Unknown uname type: $type, please message support@screenshotbot.io
fi
sh ./$INSTALLER
rm -f $INSTALLER
"))
     (defhandler (nil :uri ,(format nil "/~a.exe" name) :html nil) ()
       (hunchentoot:redirect
        (artifact-link ,(format nil "~a-win.exe" name) :cdn t)))))

(define-platform-asset "recorder")

(let ((lock (bt:make-lock)))
  (defun handle-asdf-output (op component &optional (output-num 0) )
    (bt:with-lock-held (lock)
      (let ((output (elt
                     (asdf:output-files
                      op
                      (asdf:find-system component nil))
                     output-num)))
        (when (or
               (staging-p)
               ;; in case we delete ~/.cache
               (not (path:-e output)))
          (asdf:operate op component))
        (assert (path:-e output))
        (handler-case
            (hunchentoot:handle-static-file output)
          #+lispworks
          (comm:socket-io-error (e)
            (values)))))))

(defmacro define-js (url system)
  (let ((map-url (format nil "~a.map" url)))
   `(progn
      (defhandler (nil :uri ,url :html nil) ()
        (setf (hunchentoot:content-type*) "application/javascript")
        (setf (hunchentoot:header-out :x-sourcemap) ,map-url)
        (handle-asdf-output 'asdf:compile-op ,system))
      (defhandler (nil :uri ,map-url :html nil) ()
        (handle-asdf-output 'asdf:compile-op ,system 1)))))

(define-js "/assets/js/dashboard.js" :screenshotbot.js-assets)
