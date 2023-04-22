;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/admin/site-info
  (:use #:cl)
  (:import-from #:screenshotbot/admin/core
                #:admin-app-template
                #:register-admin-menu
                #:defadminhandler)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:alexandria
                #:when-let))
(in-package :screenshotbot/admin/site-info)

(markup:enable-reader)

(defun safe-symbol-value (&rest args)
  (when-let (sym (apply #'uiop:find-symbol* args))
    (symbol-value sym)))

(defadminhandler (site-info :uri "/admin/site-info") ()
  <admin-app-template>
    <ul class= "mt-3" >
      <li>Debugger hook: ,(format nil "~s" *debugger-hook*)

      <a href= (nibble ()
                 (setf *debugger-hook* nil)
                                           (hex:safe-redirect "/admin/site-info")) >
        Reset
      </a>
      <a href= (nibble ()
                         (bt:make-thread (lambda () (error "foo")))) >
        Test Crash
      </a>
      </li>

      <li>Debug-io: ,(format nil "~s" *debug-io*)</li>
      <li>PID: ,(progn #+linux (osicat-posix:getpid))</li>
      <li>Slynk port: ,(safe-symbol-value "*ACTUAL-SLYNK-PORT*"   "SERVER/SLYNK-PREPARER" )</li>
    </ul>
  </admin-app-template>)

(register-admin-menu "Site Info" 'site-info)
