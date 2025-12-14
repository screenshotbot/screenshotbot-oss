;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/raft-redirect
    (:use :cl)
  #+ (and lispworks linux)
  (:import-from #:bknr.cluster/server
                #:wait-for-leader
                #:leader-id
                #:leaderp)
  (:export
   #:maybe-redirect-to-leader))
(in-package :screenshotbot/raft-redirect)

(defun maybe-redirect-to-leader (request)
  (declare (ignore request))
  #+bknr.cluster
  (when (and
         (boundp 'bknr.datastore:*store*)
         (not (leaderp bknr.datastore:*store*)))

    (sleep 0.2)

    (cond
      ((not (wait-for-leader bknr.datastore:*store* :timeout 6))
       (warn "Did not get a leader in time, we're probably in a read-only state")
       (log:warn "Did not get a leader in time, we're probably in a read-only state")
       (values))
      ((leaderp bknr.datastore:*store*)
       ;; If we've become the leader while we were waiting then just
       ;; continue.
       (values))
      (t
       ;; There's probably a leader, but we're not it. Respond with
       ;; 502 so that nginx knows to forward it to next backend.  It's
       ;; possible at this point there's no leader too
       ;; (wait-for-leader can return non-NIL while an election is
       ;; still happening).
       (log:info "Forwarding request to next backend (leader: ~a)" (leader-id bknr.datastore:*store*))
       (setf (hunchentoot:return-code*) 502)
       (hunchentoot:abort-request-handler)))))
