;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/ui/image
  (:use #:cl)
  (:import-from #:screenshotbot/magick
                #:ping-image-dimensions
                #:*magick*)
  (:import-from #:util/misc
                #:make-mp-hash-table))
(in-package :core/ui/image)

(named-readtables:in-readtable markup:syntax)

(defvar *dim-cache* (make-mp-hash-table :test #'equal)
  "Cache of image dimensions for every possible image.")

(defun image-dimensions (url)
  (when (str:starts-with-p "/assets/" url)
    (multiple-value-bind (result present-p)
        (gethash url *dim-cache*)
      (cond
        (present-p
         result)
        (t
         (setf (Gethash url *dim-cache*)
               (ignore-errors
                (ping-image-dimensions
                 *magick*
                 (path:catfile
                  (hunchentoot:acceptor-document-root hunchentoot:*acceptor*)
                  (str:substring 1 nil url))))))))))


(markup:deftag img-with-fallback (&key class src alt loading)
  (assert (str:containsp ".webp" src))
  (let ((dims (image-dimensions src)))
    <picture class=class >
      <source srcset= (util.cdn:make-cdn src) />
      <:img src= (util.cdn:make-cdn (str:replace-all ".webp" ".png" src))
            width= (first dims)
            height= (second dims)
            loading=loading
            alt=alt />
    </picture>))
