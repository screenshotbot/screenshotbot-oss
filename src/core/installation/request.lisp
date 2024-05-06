;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(defpackage :core/installation/request
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:core/installation/installation
                #:*secondary-installations*
                #:*installation*)
  (:export
   #:with-installation-for-request))
(in-package :core/installation/request)

(defmethod installation-matches-request-p (installation request)
  nil)

(def-easy-macro with-installation-for-request (request &fn fn)
  (declare (ignore request))
  (block outer
   (dolist (installation *secondary-installations*)
     (when (installation-matches-request-p installation request)
       (return-from outer
         (let ((*installation* installation))
           (funcall fn)))))
   (funcall fn)))
