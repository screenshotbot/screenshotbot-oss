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
  (:import-from #:hunchentoot-extensions
                #:forward-request)
  (:import-from #:core/rpc/rpc
                #:id-to-ip)
  (:export
   #:maybe-redirect-to-leader)
  (:local-nicknames (#:hex #:hunchentoot-extensions)))
(in-package :screenshotbot/raft-redirect)

(defun assert-not-in-redirect-loop (request)
  (when (hunchentoot:header-in :x-raft-forwarded request)
    (log:error "Request forwarding loop detected")
    (setf (hunchentoot:return-code*) 502)
    (hunchentoot:abort-request-handler)))

(defun maybe-redirect-to-leader (request)
  #+bknr.cluster
  (when (and
         (boundp 'bknr.datastore:*store*)
         (not (leaderp bknr.datastore:*store*))
         (not (str:starts-with-p "/raft-state"
                                 (hunchentoot:script-name request))))
    
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
       (assert-not-in-redirect-loop request)

       (let* ((leader-id (leader-id bknr.datastore:*store*))
              (leader-ip (id-to-ip leader-id))
              (port (hunchentoot:acceptor-port hunchentoot:*acceptor*))
              (url (format nil "http://~a:~a" leader-ip port)))
         (log:debug "About to forward ~a ~a to ~a"
                   (hunchentoot:request-method hunchentoot:*request*)
                   (hunchentoot:script-name hunchentoot:*request*)
                   url)
         (hex:forward-request url
                              :request request
                              :keep-current-host t
                              :extra-headers '((:x-raft-forwarded . "1"))))))))
