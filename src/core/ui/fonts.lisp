;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/ui/fonts
  (:use #:cl)
  (:export
   #:fonts-acceptor-mixin))
(in-package :core/ui/fonts)

(defclass fonts-acceptor-mixin ()
  ()
  (:documentation "Provides access to our shared fonts"))

(defvar *prefix* "/assets/fonts/")

(defvar *fonts-dir*
  "src/core/ui/assets/fonts/")

(defmethod hunchentoot:acceptor-dispatch-request ((self fonts-acceptor-mixin) request)
  (cond
    ((str:starts-with-p *prefix*
                        (hunchentoot:script-name*))
     (let ((font-name (str:substring (length *prefix*) nil (hunchentoot:script-name*))))
       (assert (not (str:starts-with-p "/" font-name)))
       (assert (not (str:containsp ".." font-name)))
       (assert (not (str:containsp "~" font-name)))
       (assert (not (str:containsp "\\" font-name)))
       (assert (not (str:containsp "//" font-name)))
       (assert (path:-d *fonts-dir*))

       (hunchentoot:handle-static-file
        (path:catfile *fonts-dir* font-name))))
    (t
     (call-next-method))))
