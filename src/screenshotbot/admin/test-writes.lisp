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
                #:nibble)
  (:import-from #:util/threading
                #:make-thread
                #:max-pool))
(in-package :screenshotbot/admin/test-writes)

(named-readtables:in-readtable markup:syntax)


(deftransaction tx-noop (msg)
  (declare (ignore msg)))

(defvar *pool*
  (make-instance 'max-pool :max 100))

(defun do-test (&key (count 1) (profile nil) (parallel nil))

  (let ((output (with-output-to-string (*trace-output*)
                  (flet ((inner-work ()
                           (let ((res (loop for i below count
                                            collect
                                               (cond
                                                 (parallel
                                                  (let ((promise (lparallel:promise)))
                                                    (make-thread
                                                     (lambda ()
                                                       (lparallel:fulfill
                                                           promise
                                                         (tx-noop "message")))
                                                     :pool *pool*)
                                                    promise))
                                                 (t
                                                  (tx-noop "message"))))))
                             (cond
                               (parallel
                                (log:info "All futures: ~S" res)
                                (time
                                 (loop for promise in res
                                       do (lparallel:force promise))))
                               (t
                                res)))))
                    (cond
                      (profile
                       (#+lispworks hcl:profile #-lispworks time
                          (inner-work)))
                      (t
                       (time
                        (inner-work))))))) )
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

    <form action= (nibble ()  (do-test :count 1000 :parallel t) ) >
      <input type= "submit" value= "Benchmark 1000 times in parallel" />
    </form>
  </admin-app-template>)

(register-admin-menu "Test Writes" 'test-writes)
