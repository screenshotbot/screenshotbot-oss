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
  (:import-from #:screenshotbot/artifacts
                #:artifact-file-name
                #:call-hooks)
  (:import-from #:util/store
                #:add-datastore-hook)
  (:import-from #:util.cdn
                #:*cdn-domain*)
  (:import-from #:screenshotbot/installation
                #:installation-domain
                #:installation)
  (:import-from #:util.cdn
                #:*cdn-domain*)
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
    (let ((util.cdn:*cdn-cache-key* (format nil "~d"
                                            (ignore-errors
                                             (file-write-date (output-file))))))
     (hunchentoot:redirect (util.cdn:make-cdn "/release/recorder-linux.sh"))))
)

(defhandler (nil :uri "/recorder-master.jar") ()
  (hex:safe-redirect "/release/recorder-master.jar"))

(defmacro handle-asdf-output (op component &optional (output-num 0))
  (let ((output-files (eval `(asdf:output-files ,op (asdf:find-component ,component nil)))))
    `(%handle-asdf-output
      ,op
      ,component
      ',output-files
      ,output-num)))

(defmacro define-css (uri asdf-target)
  (let ((map-uri (format nil "~a.map" uri)))
   `(progn
      (defhandler (nil :uri ,uri :html nil) ()
        (setf (hunchentoot:content-type*)  "text/css; charset=utf-8")
        (handle-asdf-output 'asdf:compile-op  ,asdf-target))
      (defhandler (nil :uri ,map-uri :html nil) ()
        (handle-asdf-output 'asdf:compile-op ,asdf-target 1)))))



(define-css "/assets/css/default.css" :screenshotbot.css-assets)

(defun generate-.sh (name)
  (let ((util.cdn:*cdn-domain* screenshotbot/server:*cdn-domain*))
    (let ((staging-p (not (equal "localhost" (uiop:hostname)))))
     (let ((darwin-link (artifact-link (format nil "~a-darwin" name)
                                       :cdn (not staging-p)))
           (linux-link (artifact-link (format nil "~a-linux" name)
                                      :cdn (not staging-p))))
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
"))))


(defmacro define-platform-asset (name)
  (let ((generate-fn (intern (format nil "GENERATE-~a-PLATFORM-ASSETS" (str:upcase name)))))
   `(progn
      (flet ((generate ()
               (uiop:with-staging-pathname (output
                                            (artifact-file-name ,(format nil "~a.sh" name)))
                 (with-open-file (output output :direction :output
                                                :if-exists :append)
                   (write-string (generate-.sh ,name)
                                 output)))))

        (defun ,generate-fn ()
          (generate))

        ,@ (loop for suffix in '("darwin" "linux")
                 collect
                 `(def-artifact-hook (',(intern name) ,(format nil "~a-~a"
                                                               name suffix))
                    (generate)))
        (add-datastore-hook
         ',generate-fn
         :immediate t))

      (defhandler (nil :uri ,(format nil "/~a.sh" name) :html nil) ()
        (setf (hunchentoot:content-type*) "application/x-sh")
        (hunchentoot:handle-static-file
         (artifact-file-name (format nil "~a.sh" ,name))))

      (defhandler (nil :uri ,(format nil "/~a.exe" name) :html nil) ()
        (hunchentoot:redirect
         (artifact-link ,(format nil "~a-win.exe" name) :cdn t))))))

;; (call-hooks "recorder-linux")
(define-platform-asset "recorder")
(define-platform-asset "selenium-proxy")

(defvar *lock* (bt:make-lock "assets-lock"))


(defun %handle-asdf-output (op component output-files output-num )
  (bt:with-lock-held (*lock*)
    (let ((output (elt output-files output-num)))
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
          (values))))))

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
