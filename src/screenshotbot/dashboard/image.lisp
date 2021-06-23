;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/dashboard/image
    (:use #:cl
          #:alexandria
          #:../model/image)
  (:import-from #:../server
                #:defhandler)
  (:import-from #:util
                #:find-by-oid))


(defhandler (image-blob-get :uri "/image/blob/:oid/default.png") (oid)
  (let* ((image (find-by-oid oid))
         (blob (image-blob image)))
    (assert blob)
    (setf (hunchentoot:header-out :content-type) "image/png")
    (hunchentoot:handle-static-file (bknr.datastore:blob-pathname blob))))
