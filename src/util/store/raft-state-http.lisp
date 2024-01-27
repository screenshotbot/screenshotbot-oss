;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/raft-state-http
  (:use #:cl)
  #+lispworks
  (:import-from #:bknr.cluster/server
                #:leaderp))
(in-package :util/store/raft-state-http)

(defun raft-state-request-p (request)
  ;; We want to be handle things like
  ;; `/raft-state/production` so that we can run multiple
  ;; servers on the same machine, and have nginx proxy
  ;; correctly.
  (str:starts-with-p "/raft-state"
                     (hunchentoot:script-name request)))

(defun %response ()
  #+lispworks
  (cond
    ((leaderp bknr.datastore:*store*)
     "leader")
    (t
     (setf (hunchentoot:return-code*) 400)
     "other")))

(hunchentoot:define-easy-handler (raft-state
                                  :uri #'raft-state-request-p) ()
  (%response))
