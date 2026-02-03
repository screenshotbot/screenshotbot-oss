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
                #:with-extras
                #:ignore-and-log-errors)
  (:import-from #:screenshotbot/api/model
                #:*api-version*)
  (:import-from #:screenshotbot/sdk/api-context
                #:api-feature-enabled-p)
  (:local-nicknames (#:dto #:screenshotbot/api/model)
                    (#:git #:screenshotbot/sdk/git)))
(in-package :screenshotbot/sdk/commit-graph)

(defclass commit-graph-updater ()
  ((api-context :initarg :api-context
                :reader api-context)
   (all-remote-refs :initform (make-hash-table :test #'equal)
                    :reader all-remote-refs
                    :documentation "A store of all the remote refs sent from the server")
   (refs-to-update :initform nil
                   :accessor refs-to-update
                   :documentation "A list of git-refs that will be sent to the server"))
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

(defun new-flow-enabled-p (commit-graph-updater repo)
  #-lispworks
  (declare (ignorable repo))
  #+lispworks
  (and
   (>= *api-version* 19)
   (?. supported-remote-repo-p (git:get-remote-url repo))
   (api-feature-enabled-p
    (api-context commit-graph-updater)
    :cli-shallow-clones))
  #-lispworks
  nil)

(defmethod update-commit-graph-old-style ((self commit-graph-updater)
                                          (repo null-repo)
                                          branch)
  (log:info "No repo to update commit graph"))

(defmethod update-commit-graph-old-style ((self commit-graph-updater) repo branch)
  "Update commit-graph by pulling, and then always push the top 1000 or
so changes."
  (log:info "Updating commit graph [old flow]")
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
  ;; TODO: only make this call if COMMITS is NIL. For now we're
  ;; avoiding code branches to keep it easier to test, but this should
  ;; be an easy optimization.
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
  (let ((remote-url (git:get-remote-url repo)))
    (with-extras (("remote-git-url" remote-url)
                  ("git-config-anonymized" (or (git:debug-git-config repo)
                                               "failed")))
      (let ((upload-pack (make-remote-upload-pack repo)))
        (update-from-pack
         api-context
         upload-pack
         (or (repo-link repo)
             remote-url)
         (list branch
               (git:current-branch repo))
         :current-commit (git:current-commit repo))))))

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

(defun save-refs (self refs)
  "Save the refs for reading in the future"
  (loop for  (sha . ref) in refs
        do (setf (gethash ref (all-remote-refs self)) sha)))

(defmethod maybe-push-ref-to-update ((self commit-graph-updater) sha ref)
  (when (str:starts-with-p "refs/heads/" ref)
    (push (make-instance 'dto:git-ref
                         :name (str:substring (length "refs/heads/") nil ref)
                         :sha sha)
          (refs-to-update self))))

(auto-restart:with-auto-restart (:retries 3)
  (defun read-commits-with-retries (&rest args)
    "GitLab's https endpoint is buggy, and occassionally returns 401. See T2111."
    (apply #'read-commits args)))

#+lispworks
(defmethod update-from-pack ((self commit-graph-updater)
                             (upload-pack abstract-upload-pack)
                             (repo-url string)
                             branches
                             &key current-commit)
  (log:info "Getting known refs from Screenshotbot server")
  (let ((known-refs
          ;; Technically we could combine this with the /check-wants
          ;; if we do it carefully
          (get-commit-graph-refs self repo-url)))
    (check-type known-refs list)
    (log:info "Getting git graph via git-upload-pack")
    (let ((commits (read-commits-with-retries
                    upload-pack
                    ;; TODO: also do release branches, but that will need a regex here
                    :wants (lambda (list)
                             (save-refs self list)
                             ;; This might make a network call
                             (build-wants self
                                          :current-commit current-commit
                                          :known-refs known-refs
                                          :branches branches
                                          :repo-url repo-url))
                    :haves (loop for known-ref in known-refs
                                 collect (dto:git-ref-sha known-ref))
                    :parse-parents t)))
      (%finish-update-commit-graph self
                                   :commits commits
                                   :repo-url repo-url
                                   :refs (refs-to-update self)))))

(defmethod build-wants ((self commit-graph-updater) &key current-commit
                                                      known-refs
                                                      branches
                                                      repo-url)
  "Build a list of commit SHAs that the server wants by filtering remote refs
against known refs and branches, then checking with the server which commits
are actually needed.

This function does have a side effect: it stores the the refs need to
be updated in the REFS-TO-UPDATE slot.

Returns a list of commit SHAs that should be included in the commit graph update."
  (let ((shas (remove-if
               #'null
               (list*
                current-commit
                (loop for ref being the hash-keys of (all-remote-refs self)
                        using (hash-value sha)
                      if (want-remote-ref known-refs branches
                                          sha ref)
                        collect (progn
                                  (maybe-push-ref-to-update self sha ref)
                                  sha))))))
    ;; This will make a network call
    (filter-wanted-commits
     (api-context self)
     repo-url
     shas)))

(defmethod %finish-update-commit-graph ((self commit-graph-updater)
                                        &key commits
                                          repo-url
                                          refs)
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
                  (cons "graph-json" (dto:encode-json
                                      (make-instance 'dto:commit-graph
                                                     :commits commits)))
                  (cons "refs" (dto:encode-json (or refs #())))))))


(defmethod update-commit-graph ((self commit-graph-updater) repo branch &key)
  ;; TODO: we don't need to fetch-remote branch in the new flow, but
  ;; for now keep this here. (See: T1928)
  (fetch-remote-branch repo branch)
  (log:info "Updating commit graph")
  (cond
    ((new-flow-enabled-p self repo)
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
