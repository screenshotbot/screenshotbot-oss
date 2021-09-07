;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/admin/core
  (:use #:cl
        #:alexandria
        #:screenshotbot/model/user
        #:screenshotbot/user-api
        #:screenshotbot/template)
  (:export
   #:defadminhandler
   #:register-admin-menu
   #:*index*
   #:admin-app-template)
  (:import-from
   #:screenshotbot/server
   #:defhandler
   #:with-login)
  (:use-reexport
   #:nibble
   #:markup
   #:screenshotbot/template
   #:screenshotbot/server
   #:bknr.datastore
   #:screenshotbot/ui))
(in-package :screenshotbot/admin/core)

(markup:enable-reader)

(defvar *index* nil)

(defmacro register-admin-menu (title destination)
  `(setf (assoc-value *index* ,title :test #'equal)
         ,destination))

(defmacro defadminhandler ((&rest args) params &body body)
  `(defhandler ,args ,params
     (with-login ()
      (cond
        ((not (and (current-user) (adminp (current-user))))
         <app-template>
         Please login as an admin to view this page
         </app-template>)
        (t
         (assert (adminp (current-user)))
         ,@body)))))

(deftag admin-app-template (children)
  <app-template admin=t jquery-ui=t >
  ,@children
  </app-template>)
