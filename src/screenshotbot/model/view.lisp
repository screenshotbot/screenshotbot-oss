;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/view
  (:use :cl)
  (:import-from #:auth
                #:current-user
                #:no-access-error
                #:can-view
                #:can-edit
                #:can-edit!
                #:can-view!)
  (:import-from #:screenshotbot/user-api
                #:adminp
                #:can-public-view
                #:user)
  (:import-from #:auth/view
                #:can-viewer-view)
  (:import-from #:auth/viewer-context
                #:site-admin-viewer-context)
  (:export
   #:can-edit
   #:can-edit!
   #:can-public-view
   #:can-view
   #:can-view!))
(in-package :screenshotbot/model/view)

;; This file adds logic to check if a specific object can be viewed by
;; the given user

(defmethod can-view :around (obj (user user))
  ;; TODO: remove
  (call-next-method))

(defmethod can-viewer-view :around ((vc site-admin-viewer-context)
                                    obj)
  (or
   ;; always call next method to make sure that code gets tested.
   (call-next-method)
   t))
