;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/assets
    (:use #:cl
          #:alexandria
          #:./artifacts)
  (:import-from #:./server
                #:staging-p
                #:defhandler)
  (:export #:prepare-delivered-asset-map
           #:define-css
           #:define-js
           #:append-asset-map))

(named-readtables:in-readtable :interpol-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun asset-output (component output)
    (format nil "assets/~a/~a.~a"
            (asdf:component-name component)
            (pathname-name output)
            (pathname-type output)))

  (defun asset-copy-file (asset-file output)
    (unless (path:-d output) ;; remove directories :/
      (log:info "Copying ~a to ~a" output asset-file)
      (ensure-directories-exist asset-file)
      (uiop:copy-file output asset-file)))

  (defun prepare-delivered-asset-map (top-level-system &key (copy t))
    (let ((response nil))
     (let ((seen (make-hash-table)))
       (labels ((prep (component)
                  (when component
                    (let ((component-name (intern (str:upcase (asdf:component-name component)) "KEYWORD")))
                      (when (typep component 'build-utils:web-asset)
                        (log:debug "Got component ~s" component-name)

                        (let ((asset-files (loop for output in (asdf:output-files 'asdf:compile-op component)
                                                 collect
                                                 (let ((asset-file (asset-output component output)))
                                                   (when copy
                                                     (asset-copy-file asset-file output))
                                                   asset-file))))
                          (setf (assoc-value response component-name)
                                asset-files)))))
                  (when (and
                         component
                         (not (gethash component seen))
                         ;; remove some biggish dependencies
                         (let ((component (asdf:component-name component)))
                           (str:starts-with-p "screenshotbot" component)))
                    (setf (gethash component seen) t)
                    (let ((children (asdf:system-depends-on component)))
                      (dolist (child children)
                        (prep (asdf:find-component child nil)))))))
         (prep (asdf:find-component top-level-system nil))
         response)))))

;; At compile time, we generate *delivered-asset-compile-map*. But we
;; also need that outputted in the generated fasl files. So that's
;; what's happening here.
;;
;; Warning: if you need to change this line, you might also need to
;; change the corresponding line in pro/assets
(defvar *delivered-asset-map* nil)

(defun append-asset-map (asset-map)
  (loop for (key . value) in asset-map
        do
        (setf (assoc-value *delivered-asset-map* key) value)))

(append-asset-map
 `#.(prepare-delivered-asset-map
     :screenshotbot
     :copy nil))

(defhandler (static-recorder-master :uri "/release/recorder-master.jar") ()
  (hunchentoot:handle-static-file #P "/home/arnold/builds/silkwrmsdk/binary/build/libs/binary-0.1.0-all.jar"))


(flet ((output-file ()  (asdf:output-file 'asdf:compile-op
                                          (asdf:find-component :screenshotbot.sdk.deliver "deliver-sdk"))) )
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
        (handle-asdf-output ,asdf-target))
      (defhandler (nil :uri ,map-uri :html nil) ()
        (handle-asdf-output ,asdf-target 2)))))

(define-css "/assets/css/default.css" :screenshotbot.css-assets)


(defhandler (nil :uri "/recorder.sh" :html nil) (no-cdn)
  (setf (hunchentoot:content-type*) "application/x-sh")
  (let ((darwin-link (artifact-link "recorder-darwin" :cdn (not no-cdn)))
        (linux-link (artifact-link "recorder-linux" :cdn (not no-cdn))))
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


(let ((lock (bt:make-lock)))
  (defun handle-asdf-output (component &optional (output-num 0))
    (cond
      (util:*delivered-image*
       (assert (symbolp component))
       (let ((output (assoc-value *delivered-asset-map* component)))
         (hunchentoot:handle-static-file (elt output output-num))))
      (t
       (bt:with-lock-held (lock)
         (let ((output (elt
                        (asdf:output-files
                         'asdf:compile-op
                         (asdf:find-system component nil))
                        output-num)))
           (when (or
                  (staging-p)
                  ;; in case we delete ~/.cache
                  (not (path:-e output)))
             (asdf:compile-system component))
           (assert (path:-e output))
           (hunchentoot:handle-static-file output)))))))

(defmacro define-js (url system)
  (let ((map-url (format nil "~a.map" url)))
   `(progn
      (defhandler (nil :uri ,url :html nil) ()
        (setf (hunchentoot:content-type*) "application/javascript")
        (setf (hunchentoot:header-out :x-sourcemap) ,map-url)
        (handle-asdf-output ,system))
      (defhandler (nil :uri ,map-url :html nil) ()
        (handle-asdf-output ,system 1)))))

(define-js "/assets/js/dashboard.js" :screenshotbot.js-assets)
