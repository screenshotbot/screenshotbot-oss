;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/cached-commit-graph-updater
  (:use #:cl)
  (:import-from #:screenshotbot/sdk/commit-graph
                #:commit-graph-updater
                #:all-remote-refs
                #:update-commit-graph)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class)
  (:local-nicknames (#:dto #:screenshotbot/api/model))
  (:export
   #:cached-commit-graph-updater))
(in-package :screenshotbot/sdk/cached-commit-graph-updater)

(defvar *cache-version* 1
  "Bump this if the format of CACHE-CONTENTS changes. Files with any
other version are treated as a cache miss.")

(defclass cache-contents ()
  ((version :initarg :version
            :initform nil
            :json-key "version"
            :json-type (or null :number)
            :reader cache-version)
   (refs :initarg :refs
         :initform nil
         :json-key "refs"
         :json-type (:list dto:git-ref)
         :documentation "The remote branches. The names are without the
refs/heads/ prefix, like in the rest of the API."
         :reader cache-refs))
  (:metaclass ext-json-serializable-class)
  (:documentation "What's stored in a cache file, as JSON."))

(defvar *max-cache-file-age* (* 2 3600)
  "Cache files older than this many seconds get deleted. They're only
ever read in the hour they were written in.")

(defun default-commit-graph-cache-dir ()
  (uiop:xdg-cache-home "screenshotbot/commit-graph/"))

(defclass cached-commit-graph-updater (commit-graph-updater)
  ((build-url :initarg :build-url
              :initform nil
              :reader build-url)
   (commit :initarg :commit
           :initform nil
           :reader commit)
   (cache-dir :initarg :cache-dir
              :initform nil
              :documentation "Defaults to DEFAULT-COMMIT-GRAPH-CACHE-DIR,
which is resolved lazily, since it can signal an error (e.g. for a
relative XDG_CACHE_HOME)."))
  (:documentation "A commit-graph-updater that caches the result of
UPDATE-COMMIT-GRAPH on disk, keyed on the build URL, the current commit
and the current hour. Sharded CI builds run the CLI many times for the
same commit, and only the first one needs to go through the full
flow. The others just restore the remote refs from the cache.

Concurrent processes might all miss the cache, in which case each of
them goes through the full flow, and the last one to finish wins.

Known limitation, accepted for now: the key does not include anything
else that the flow depends on, namely the API hostname and API key (i.e.
which installation and company we upload to), the repo URL, and the
main branch and base branch passed to UPDATE-COMMIT-GRAPH. So if a
single build runs the CLI at the same commit against two servers, or
with two different main branches, only the first run uploads the
commit graph, and the others reuse its refs.

Any error while using the cache (e.g. a bad cache directory, or an
unreadable cache file) is logged and treated as a cache miss: the cache
must never fail a customer's CI run."))

(defmethod cache-dir ((self cached-commit-graph-updater))
  (or (slot-value self 'cache-dir)
      (default-commit-graph-cache-dir)))

(defmacro with-cache-errors-logged ((action) &body body)
  "Run BODY, and if it signals an error, log it and return NIL."
  `(handler-case
       (progn ,@body)
     (error (e)
       (log:warn "Commit-graph cache: could not ~a: ~a" ,action e)
       nil)))

(defun hour-start (time)
  "TIME is a universal-time. Returns the universal-time of the start of
that hour, e.g. 14:23:10 becomes 14:00:00."
  (* 3600 (floor time 3600)))

(defmethod cache-key ((self cached-commit-graph-updater) &key (now (get-universal-time)))
  "See the class documentation for what's deliberately not part of the key."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (flex:string-to-octets
     (format nil "~a~%~a~%~a"
             (build-url self)
             (commit self)
             (hour-start now))
     :external-format :utf-8))))

(defmethod cache-file ((self cached-commit-graph-updater) &key (now (get-universal-time)))
  (make-pathname :name (cache-key self :now now)
                 :type "json"
                 :defaults (cache-dir self)))

(defmethod cacheablep ((self cached-commit-graph-updater))
  (not (or (str:emptyp (build-url self))
           (str:emptyp (commit self)))))

(defvar *branch-prefix* "refs/heads/")

(defun branch-ref-p (ref)
  (str:starts-with-p *branch-prefix* ref))

(defmethod branch-refs ((self cached-commit-graph-updater))
  "The (sha . ref) pairs for the branches in ALL-REMOTE-REFS. Those are
the only refs that are read after the flow (see REMOTE-BRANCH-SHA), so
we don't cache the rest (tags, GitHub's refs/pull/*, ...)."
  (loop for ref being the hash-keys of (all-remote-refs self)
          using (hash-value sha)
        if (branch-ref-p ref)
          collect (cons sha ref)))

(defmethod read-cache-file ((self cached-commit-graph-updater) file)
  "If FILE is a valid cache file, fill in the slots from it and return T.
If it doesn't exist, or has a different version, return NIL and don't
touch the slots. Signals an error if it can't be parsed."
  (when (probe-file file)
    (let ((contents (dto:decode-json
                     (uiop:read-file-string file :external-format :utf-8)
                     'cache-contents)))
      (cond
        ((not (eql *cache-version* (cache-version contents)))
         (log:info "Ignoring commit-graph cache file with version ~a: ~a"
                   (cache-version contents) file)
         nil)
        (t
         ;; Validate everything before touching the slots
         (let ((refs (loop for git-ref in (cache-refs contents)
                           collect (cons (format nil "~a~a" *branch-prefix*
                                                 (ensure-non-empty-string (dto:git-ref-name git-ref)))
                                         (ensure-non-empty-string (dto:git-ref-sha git-ref))))))
           (clrhash (all-remote-refs self))
           (loop for (ref . sha) in refs
                 do (setf (gethash ref (all-remote-refs self)) sha))
           t))))))

(defun ensure-non-empty-string (value)
  "Signal an error unless VALUE is a non-empty string, and return it."
  (unless (and (stringp value)
               (not (str:emptyp value)))
    (error "Invalid value in commit-graph cache file: ~s" value))
  value)

(defmethod write-cache-file ((self cached-commit-graph-updater) file)
  "Atomically write the branch refs to FILE as JSON. The staging file
gets a unique name, so concurrent writers don't step on each other: the
last rename wins."
  (ensure-directories-exist file)
  (let ((json (dto:encode-json
               (make-instance
                'cache-contents
                :version *cache-version*
                :refs (loop for (sha . ref) in (branch-refs self)
                            collect (make-instance
                                     'dto:git-ref
                                     :name (subseq ref (length *branch-prefix*))
                                     :sha sha))))))
    (uiop:with-staging-pathname (staging file)
      (with-open-file (stream staging :direction :output
                                      :if-exists :supersede
                                      :external-format :utf-8)
        (write-string json stream)))))

(defmethod prune-cache-dir ((self cached-commit-graph-updater) &key (now (get-universal-time)))
  "Delete cache files older than *MAX-CACHE-FILE-AGE*. This also cleans
up staging files left behind by processes that died mid-write."
  (dolist (file (directory (make-pathname :name :wild :type "json"
                                          :defaults (cache-dir self))))
    ;; Another process might be pruning the same files at the same
    ;; time, so errors for a single file are expected.
    (ignore-errors
     (when (< (file-write-date file) (- now *max-cache-file-age*))
       (delete-file file)))))

(defmethod update-commit-graph :around ((self cached-commit-graph-updater) repo branch
                                        &key &allow-other-keys)
  (let ((file (when (cacheablep self)
                ;; Compute the file once, so that the hour doesn't
                ;; change under us
                (with-cache-errors-logged ("find the cache file")
                  (cache-file self)))))
    (cond
      ((and file
            (with-cache-errors-logged ("read the cache file")
              (read-cache-file self file)))
       (log:info "Using the cached commit graph from ~a" file)
       t)
      (t
       (multiple-value-prog1
           (call-next-method)
         ;; If we have no remote branch refs (old flow, or the new
         ;; flow failed), don't cache: the next process should try
         ;; again, and in the old flow it needs the git fetch from
         ;; :always-git-fetch.
         (when (and file (branch-refs self))
           (with-cache-errors-logged ("write the cache file")
             (write-cache-file self file))
           (with-cache-errors-logged ("prune the cache directory")
             (prune-cache-dir self))))))))
