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
                #:make-default-store))
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
              :description "The other peers (excluding this one). Command separated IP addresses without port numbers."))))

(defun cluster-init/handler (cmd)
 (let ((store (str:ensure-suffix "/" (clingon:getopt cmd :store))))
   (init-raft-config store
                     (remove-if #'str:emptyp
                                (str:split "," (or (clingon:getopt cmd :other-peers)
                                                   ""))))))


(defun init-raft-config (store other-peers)
  (let ((raft-config (path:catfile store "raft-config.lisp")))
    (uiop:with-staging-pathname (raft-config raft-config)
      (with-open-file (stream raft-config :direction :output :if-exists :append)
        (let ((*package* (find-package :cl-user)))
          (format stream "~s"
                  `(make-default-store
                    'ec2-store
                    :group "screenshotbot"
                    :data-path ,(namestring "~/raft-data/")
                    :port 7070
                    :ips (list
                          ,(ec2-get-local-ipv4)
                          ,@(loop for peer in other-peers
                                  collect (format nil "~a" peer))))))))
    (log:info "Updated ~a" raft-config)))

