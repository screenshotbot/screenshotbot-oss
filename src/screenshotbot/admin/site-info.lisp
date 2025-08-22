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
                #:when-let)
  (:import-from #:util/timeago
                #:timeago)
  #+ (and bknr.cluster (not :screenshotbot-oss))
  (:local-nicknames (#:cluster #:cluster/cluster)))
(in-package :screenshotbot/admin/site-info)

(named-readtables:in-readtable markup:syntax)

(defvar *boot-time* (get-universal-time))

#+lispworks
(lw:define-action "When starting image" "Update boot time"
  (lambda ()
    (setf *boot-time* (get-universal-time))))

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
      <li>PID: ,(progn #+linux (util/posix:getpid))</li>
      <li>Booted at: ,(timeago :timestamp *boot-time*)</li>
      <li>Slynk port: ,(safe-symbol-value "*ACTUAL-SLYNK-PORT*"   "SERVER/SLYNK-PREPARER" )</li>
      <li>Hostname: ,(uiop:hostname)</li>
      <li>Nginx downstream: ,(hunchentoot:remote-addr*)</li>
      <li>Leader term:
        ,(progn
           #+:bknr.cluster
           (bknr.cluster/server:leader-term bknr.datastore:*store*))
      </li>

      <li>Cluster config:
        ,@(progn
           #+ (and bknr.cluster (not :screenshotbot-oss))
           (let ((peers (bknr.cluster/server:list-peers bknr.datastore:*store*)))
             (list
              (str:join ","
                        peers)
              (render-peer-info peers))))
      </li>

      <li>Features: ,(progn *features*)</li>
    </ul>

    Helpful links:
      <ul>
        <li><a href= "/admin/test-writes">Test writes</a></li>
        <li><a href= "/admin/thread-list">Threads</a></li>
      </ul>
  </admin-app-template>)

#+ (and bknr.cluster (not :screenshotbot-oss))
(defun render-peer-info (peers)
  <table class= "table border" >
    ,@(loop for peer in peers
            for instance = (cluster:get-instance-by-ip (first (str:rsplit ":"  peer :limit 4)))
             collect
             <tr>
               <td>,(cluster:instance-name instance)</td>
               <td>,(progn peer)</td>
               <td>,(cluster:private-ip instance)</td>
               <td>,(cluster:ipv6-address instance)</td>
               <td><timeago timestamp= (local-time:parse-timestring (cluster:launch-time instance)) /></td>
             </tr>)
  </table>)

(defadminhandler (thread-list :uri "/admin/thread-list") ()
  <admin-app-template>
    <table>
      ,@ (loop for thread in (bt:all-threads)
               collect
               <tr>
                 <td>
                   ,(format nil "~a" thread)
                 </td>
               </tr>)
    </table>
  </admin-app-template>)

(register-admin-menu "Site Info" 'site-info)
(register-admin-menu "Thread list" 'thread-list)
