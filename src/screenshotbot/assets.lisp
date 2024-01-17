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
                #:acceptor
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
                #:installation-cdn
                #:desktop-installation
                #:pre-compiled-assets
                #:installation-domain
                #:installation)
  (:import-from #:core/installation/installation
                #:installation-domain)
  (:import-from #:core/ui/assets
                #:handle-asdf-output
                #:%handle-asdf-output
                #:define-css
                #:*asset-list*
                #:ensure-asset)
  (:export
   #:define-css
   #:*asset-list*
   #:ensure-asset
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
     (hunchentoot:redirect (util.cdn:make-cdn "/release/recorder-linux.sh")))))

(defhandler (nil :uri "/recorder-master.jar") ()
  (hex:safe-redirect "/release/recorder-master.jar"))

(define-css acceptor "/assets/css/default.css" :screenshotbot.css-assets)

(defun generate-.sh (name)
  (let ((domain (or
                 (installation-cdn (installation))
                 ;; A hack for staging:
                 (installation-domain (installation)))))
    (flet ((make-link (platform)
             (format nil "~a/artifact/${VERSION}~a-~a" domain name platform))
           (fetch (link)
             (format nil "$CURL --progress-bar ~a --output $INSTALLER"
                     link)))
     (let* ((darwin-link (make-link "darwin"))
            (linux-link (make-link "linux"))
            (arm64-link (format nil "~a-arm64" linux-link))
            (domain (installation-domain (installation))))
       #?"#!/bin/sh
set -e
set -x

type=`uname`

INSTALLER=screenshotbot-installer.sh
CURL=\"curl --retry 3 \"
VERSION=`$CURL --fail ${domain}/recorder-version/current || true`

if [ $type = \"Linux\" ] ; then
  if [ \"`uname -m`\" = \"aarch64\" ] ; then
    ${(fetch arm64-link)}
  else
    ${(fetch linux-link)}
  fi
elif [ $type = \"Darwin\" ] ; then
  ${(fetch darwin-link)}
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
                 for full-name = (format nil "~a-~a"
                                         name suffix)
                 collect
                 `(def-artifact-hook (',(intern full-name) ,full-name)
                    (generate)))
        (add-datastore-hook
         ',generate-fn
         :immediate t))

      (defhandler (nil :uri ,(format nil "/~a.sh" name)) ()
        (setf (hunchentoot:content-type*) "application/x-sh")
        (hunchentoot:handle-static-file
         (artifact-file-name (format nil "~a.sh" ,name))))

      (defhandler (nil :uri ,(format nil "/~a.exe" name)) ()
        (hunchentoot:redirect
         (artifact-link ,(format nil "~a-win.exe" name) :cdn t))))))

;; (call-hooks "recorder-linux")
(define-platform-asset "recorder")
(define-platform-asset "selenium-proxy")

(defhandler (recorder-incorrect :uri "/recorder") ()
  (setf (hunchentoot:return-code*) 404)
  "Page not found. Did you mean /recorder.sh? Or for Windows, this would be /recorder-win.exe")

(defvar *lock* (bt:make-lock "assets-lock"))


(defmethod %handle-asdf-output ((installation desktop-installation)
                                op
                                component
                                output-files
                                output-num)
  (let ((cache (pre-compiled-assets installation)))
    (elt (gethash component cache) output-num)))

(defmacro define-js (url system)
  (let ((map-url (format nil "~a.map" url)))
    `(progn
       (ensure-asset ,system)
       (defhandler (nil :uri ,url) ()
         (setf (hunchentoot:content-type*) "application/javascript")
         (setf (hunchentoot:header-out :x-sourcemap) ,map-url)
         (handle-asdf-output 'asdf:compile-op ,system))
       (defhandler (nil :uri ,map-url) ()
         (handle-asdf-output 'asdf:compile-op ,system 1)))))

(define-js "/assets/js/dashboard.js" :screenshotbot.js-assets)
