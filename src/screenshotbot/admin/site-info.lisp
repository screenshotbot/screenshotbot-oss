;;;; -*- coding: utf-8 -*-
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
  (:import-from #:bknr.cluster/server
                #:peer-status-consecutive-error-times
                #:peer-status-next-index
                #:get-peer-status)
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
  (let* ((instances
           (loop for peer in peers
                 collect
                 (cluster:get-instance-by-ip (first (str:rsplit ":"  peer :limit 4)))))
         (target-list
           (cluster:target-group-target-list
            (get-current-target-group instances))))
      <table class= "table border" >
    <tr>
      <th>Instance</th>
      <th>Peer</th>
      <th>Private IP</th>
      <td>In target group</td>
      <th>IPv6</th>
      <th>Next Index</th>
      <th>Error Times</th>
      <th>Launch Time</th>
    </tr>
    ,@(loop for peer in peers
            for instance in instances
             collect
            (render-peer peer instance target-list))
    ,@ (loop for instance in (list-current-security-group instances)
             collect
             (render-peer nil instance target-list))
  </table>))

#+ (and bknr.cluster (not :screenshotbot-oss))
(defun get-current-security-group (instances)
  (let ((security-groups (cluster:security-group-names (car instances))))
    (loop for sg in security-groups
          unless (equal sg "ipv6-ssh")
            return sg)))

#+ (and bknr.cluster (not :screenshotbot-oss))
(defun get-current-target-group (instances)
  (let ((target-groups (cluster:get-all-target-groups)))
    (loop for tg in target-groups
          when (some (lambda (instance)
                      (str:s-member (cluster:target-group-target-list tg)
                                    (cluster:instance-id instance)))
                    instances)
            return tg)))

#+ (and bknr.cluster (not :screenshotbot-oss))
(defun list-current-security-group (instances)
  (let ((old-ids (mapcar #'cluster:instance-id instances)))
   (let ((sg (get-current-security-group instances)))
     (let ((all-instances (cluster:list-security-group sg)))
       (loop for new-instance in all-instances
             unless (str:s-member old-ids (cluster:instance-id new-instance))
               collect new-instance)))))

#+ (and bknr.cluster (not :screenshotbot-oss))
(defun render-peer (peer instance target-list)
  (let ((peer-status
          ;; This next ignore-errors can be removed once every server is restarted
          (when peer
           (ignore-errors (get-peer-status bknr.datastore:*store* peer)))))
    <tr>
      <td><a href=(cluster:instance-url instance) >,(cluster:instance-name instance)</a></td>
      <td>,(progn peer)</td>
      <td>,(cluster:private-ip instance)</td>
      <td>,(cond
             ((str:s-member target-list (cluster:instance-id instance))
              "✓")
             (t
             "X"))
      </td>
      <td>,(cluster:ipv6-address instance)</td>
      <!-- peer-status will be NIL for the leader -->
      <td>,(ignore-errors (peer-status-next-index peer-status)) </td>
      <td>,(ignore-errors (peer-status-consecutive-error-times peer-status))</td>
      <td><timeago timestamp= (local-time:parse-timestring (cluster:launch-time instance)) /></td>
    </tr>))

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
