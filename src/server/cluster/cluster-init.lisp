;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :server/cluster/cluster-init
  (:use #:cl)
  (:import-from #:util/store/store
                #:ec2-get-local-ipv4
                #:ec2-store
                #:make-default-store)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:alexandria
                #:assoc-value))
(in-package :server/cluster/cluster-init)

(defun cluster-init/command ()
  (clingon:make-command
   :name "init"
   :description "Creates a raft-config.lisp in the given store directory, with only the current machine."
   :handler 'cluster-init/handler
   :options (list
             (clingon:make-option
              :string
              :key :store
              :description "The store directory to create the raft-config.lisp in"
              :long-name "store")
             (clingon:make-option
              :string
              :key :other-peers
              :long-name "other-peers"
              :description "Deprecated. Keep blank."))))

(defun cluster-init/handler (cmd)
  (log:config :debug)
  (let ((store (str:ensure-suffix "/" (clingon:getopt cmd :store))))
    (init-raft-config store
                      (remove-if #'str:emptyp
                                (str:split "," (or (clingon:getopt cmd :other-peers)
                                                   ""))))))

(defun id-to-ip (conf)
  (first (str:rsplit ":" conf :limit 4)))

(defun init-raft-config (store other-peers)
  (let ((raft-config (path:catfile store "raft-config.lisp"))
        (my-ip (ec2-get-local-ipv4)))
    (cond
      ((path:-e raft-config)
       ;; We're already dealing with an existing cluster
       (log:info "Growing existing cluster")
       (grow-cluster my-ip
                     (str:trim (uiop:read-file-string (path:catfile store "leader.txt")))))
      (t
       (make-dirs)
       (uiop:with-staging-pathname (raft-config raft-config)
         (with-open-file (stream raft-config :direction :output :if-exists :append)
        (let ((*package* (find-package :cl-user)))
          (format stream "~s"
                  `(make-default-store
                    'ec2-store
                    :group "screenshotbot"
                    :data-path ,(namestring "~arnold/raft-data/")
                    :port 7070
                    :ips (list
                          ,my-ip
                          ,@(loop for peer in other-peers
                                  collect (format nil "~a" peer))))))))
       (log:info "Updated ~a" raft-config)))))

(defun %shell (cmd)
  (uiop:run-program
   cmd
   :output t
   :error-output t))

(defun make-dirs ()
  (%shell "sudo mkdir /mnt/efs/fs1/screenshotbot -p")
  (%shell "sudo chown arnold:arnold /mnt/efs/fs1/screenshotbot")
  (%shell "sudo -u arnold mkdir -p /mnt/efs/fs1/screenshotbot/object-store")
  (%shell "sudo -u arnold mkdir -p /mnt/efs/fs1/screenshotbot/logs"))

(defun raft-state (peer)
  (json:decode-json-from-string
   (http-request (format nil "http://~a:4001/raft-state?full=true"
                         (id-to-ip peer)))))

(defun peer-instance-name (peer)
  (assoc-value
   (raft-state peer)
   :name))

(defun peer-map (peers)
  "Builds a map from name to peer"
  (let ((map (fset:empty-map)))
    (dolist (peer peers)
      (fset:includef map (peer-instance-name peer)
                     peer))
    map))

(defun map-to-peers (map)
  (str:join ","
            (fset:convert 'list (fset:range map))))

(defun grow-cluster (my-ip leader-id)
  ;; First, get the current cluster config:
  (log:info "Waiting 30s to make sure the screenshotbot service has started")
  (sleep 30)
  (let* ((config (raft-state leader-id))
         (peers (assoc-value config :peers))
         (old-peers (peer-map peers))
         (new-peers (fset:with
                     old-peers
                     (str:trim (uiop:read-file-string "/etc/screenshotbot-node-name"))
                     (format nil "~a:7070:0" my-ip))))
    (log:info "Running braft_cli add_peer")
    (%shell (format nil
                    "braft_cli change_peers --group=screenshotbot --conf=~a --new_peers=~a "
                    (map-to-peers old-peers)
                    (map-to-peers new-peers)))))

