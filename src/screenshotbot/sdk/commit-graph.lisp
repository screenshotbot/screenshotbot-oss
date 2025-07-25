;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/commit-graph
  (:use #:cl)
  (:import-from #:screenshotbot/sdk/request
                #:request)
  (:import-from #:screenshotbot/sdk/git
                #:null-repo
                #:read-graph
                #:fetch-remote-branch
                #:repo-link)
  #+lispworks
  (:import-from #:screenshotbot/sdk/git-pack
                #:abstract-upload-pack
                #:remote-ref-equals
                #:read-commits
                #:make-remote-upload-pack
                #:upload-pack
                #:supported-remote-repo-p)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:util/threading
                #:ignore-and-log-errors)
  (:import-from #:screenshotbot/api/model
                #:*api-version*)
  (:local-nicknames (#:dto #:screenshotbot/api/model)
                    (#:git #:screenshotbot/sdk/git)))
(in-package :screenshotbot/sdk/commit-graph)

(defclass commit-graph-updater ()
  ((api-context :initarg :api-context
                :reader api-context))
  (:documentation "The commit graph updater will eventually be stateful, in particular
to cache the refs."))

(defmethod get-commit-graph-refs ((self commit-graph-updater)
                                  repo)
  (dto:decode-json
   (request
    (api-context self)
    "/api/commit-graph/refs"
    :method :get
    :decode-response nil
    :parameters (list
                 (cons "repo-url" repo)))
   '(:list dto:git-ref)))

(defun locally-rebased-p (repo &key override-commit-hash)
  (or
   (not override-commit-hash)
   (equal override-commit-hash (git:current-commit repo))))

(defun new-flow-enabled-p (repo &key override-commit-hash)
  (declare (ignorable repo override-commit-hash))
  #+lispworks
  (and
   (>= *api-version* 19)
   (?. supported-remote-repo-p (git:get-remote-url repo))
   (equal "true" (uiop:getenv "SCREENSHOTBOT_ENABLE_UPLOAD_PACK"))
   (locally-rebased-p repo :override-commit-hash override-commit-hash))
  #-lispworks
  nil)

(defmethod update-commit-graph-old-style ((self commit-graph-updater) repo branch)
  "Update commit-graph by pulling, and then always push the top 1000 or
so changes."
  (log:info "Updating commit graph")
  (let* ((dag (read-graph repo))
         (json (with-output-to-string (s)
                 (dag:write-to-stream dag s))))
    (request
     (api-context self)
     "/api/commit-graph"
     :method :post
     :parameters (list
                  (cons "repo-url" (repo-link repo))
                  (cons "branch" branch)
                  (cons "graph-json" json)))))

(defmethod filter-wanted-commits (api-context repo-url commits)
  "Check if the server needs these commits, and returns the list of
commits that are needed."
  (let ((commits (remove-duplicates commits :test #'equal)))
    (json:decode-json-from-string
     (request
      api-context
      "/api/commit-graph/check-wants"
      :decode-response nil
      :method :get
      :parameters `(("repo-url" . ,repo-url)
                    ("shas" . ,(json:encode-json-to-string commits)))))))

#+lispworks
(defun update-commit-graph-new-style (api-context repo branch)
  (let ((upload-pack (make-remote-upload-pack repo)))
    (update-from-pack
     api-context
     upload-pack
     (git:get-remote-url repo)
     (list branch
           (git:current-branch repo))
     :current-commit (git:current-commit repo))))


(defun ref-in-sync-p (known-refs sha ref)
  "At this point, we know we're interested in this ref. What we need to know is if this is in sync with the server"
  (declare (ignore ref))
  (loop for known-ref in known-refs
        ;; Weird, note that we actually don't care about ref here,
        ;; since if this commit was seen on the server, then that's
        ;; good enough.
        if (equal (dto:git-ref-sha known-ref) sha)
          return t))

#+lispworks
(defun want-remote-ref (known-refs branches sha ref)
  (check-type known-refs list)
  (loop for branch in branches
        if (remote-ref-equals branch ref)
          return (not (ref-in-sync-p known-refs sha ref))))

#+lispworks
(defmethod update-from-pack ((self commit-graph-updater)
                             (upload-pack abstract-upload-pack)
                             (repo-url string)
                             branches
                             &key current-commit)
  (log:info "Getting known refs from Screenshotbot server")
  (let ((known-refs (get-commit-graph-refs self repo-url))
        (refs nil))
    (check-type known-refs list)
    (log:info "Getting git graph via git-upload-pack")
    (flet ((maybe-push-ref (sha ref)
             (when (str:starts-with-p "refs/heads/" ref)
               (push (make-instance 'dto:git-ref
                                    :name (str:substring (length "refs/heads/") nil ref)
                                    :sha sha)
                     refs))))
     (let ((commits (read-commits
                     upload-pack
                     ;; TODO: also do release branches, but that will need a regex here
                     :wants (lambda (list)
                              (filter-wanted-commits
                               (api-context self)
                               repo-url
                               (remove-if
                                #'null
                                (list*
                                 current-commit
                                 (loop for (sha . ref) in list
                                       if (want-remote-ref known-refs branches
                                                           sha ref)
                                         collect (progn
                                                   (maybe-push-ref sha ref)
                                                   sha))))))
                     :haves (loop for known-ref in known-refs
                                  collect (dto:git-ref-sha known-ref))
                     :parse-parents t)))
       (let ((commits (loop for (sha . parents) in commits
                            collect (make-instance 'dto:commit
                                                   :sha sha
                                                   :parents parents))))
         (log:info "Updating git commit-graph")
         (request
          (api-context self)
          "/api/commit-graph"
          :method :post
          :parameters (list
                       (cons "repo-url" repo-url)
                       (cons "graph-json" (dto:encode-json (or commits #())))
                       (cons "refs" (dto:encode-json (or refs #()))))))))))


(defmethod update-commit-graph ((self commit-graph-updater) repo branch
                                &key override-commit-hash)
  ;; TODO: we don't need to fetch-remote branch in the new flow, but
  ;; for now keep this here. (See: T1928)
  (fetch-remote-branch repo branch)
  (log:info "Updating commit graph")
  (cond
    ((new-flow-enabled-p repo :override-commit-hash override-commit-hash)
     (or
      (ignore-and-log-errors ()
        (trivial-timeout:with-timeout (600)
          (update-commit-graph-new-style self repo branch))
        (log:debug "New commit graph uploaded successfully")
        t)
      (progn
        (warn "Reverting to old commit-graph flow")
        (update-commit-graph-old-style self repo branch))))
    (t
     (log:info "Using old flow for commit-graph")
     #+nil
     (warn "Using the old commit-graph flow for ~a" (git:get-remote-url repo))
     (update-commit-graph-old-style self repo branch))))

(defmethod update-commit-graph (self (repo null-repo) branch &key &allow-other-keys)
  (log:info "Not updating the commit graph, since there's no repo"))
