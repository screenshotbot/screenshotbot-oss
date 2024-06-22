;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/site-admin
  (:use #:cl)
  (:import-from #:auth/request
                #:make-default-viewer-context)
  (:import-from #:screenshotbot/server
                #:request)
  (:import-from #:screenshotbot/user-api
                #:adminp
                #:user)
  (:import-from #:auth/viewer-context
                #:site-admin-viewer-context))
(in-package :screenshotbot/site-admin)


(defmethod make-default-viewer-context :around ((request request)
                                                (user user))
  (cond
    ((and (adminp user)
          (auth:session-value :site-admin-privileges-enabled))
     (make-instance 'site-admin-viewer-context :user user))
    (t
     (call-next-method))))
