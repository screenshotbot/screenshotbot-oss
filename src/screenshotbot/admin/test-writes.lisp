;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/admin/test-writes
  (:use #:cl)
  (:import-from #:screenshotbot/admin/core
                #:register-admin-menu
                #:admin-app-template
                #:defadminhandler)
  (:import-from #:bknr.datastore
                #:deftransaction)
  (:import-from #:nibble
                #:nibble))
(in-package :screenshotbot/admin/test-writes)

(named-readtables:in-readtable markup:syntax)


(deftransaction tx-noop (msg)
  (declare (ignore msg)))

(defun do-test (&key (count 1) (profile nil))
  (let ((output (with-output-to-string (*trace-output*)
                  (flet ((inner-work ()
                           (loop for i below count do
                             (tx-noop "message"))))
                   (if profile
                       (#+lispworks hcl:profile #-lispworks time
                        (inner-work))
                       (time
                        (inner-work)))))) )
    <html>
      <body>
        Success. <a href= "/admin/test-writes">Go Back.</a>

        <pre>,(progn output)</pre>
      </body>
    </html>))

(defadminhandler (test-writes :uri "/admin/test-writes") ()
  <admin-app-template>
    <form action= (nibble ()  (do-test) ) >
      <input type= "submit" />
    </form>

    <form action= (nibble ()  (do-test :count 100) ) >
      <input type= "submit" value= "Benchmark 100 times" />
    </form>

    <form action= (nibble ()  (do-test :count 1000 :profile t) ) >
      <input type= "submit" value= "Profile 1000 times" />
    </form>
  </admin-app-template>)

(register-admin-menu "Test Writes" 'test-writes)
