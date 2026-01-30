;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/sdk/git
  (:use #:cl
        #:alexandria)
  (:import-from #:util/health-check
                #:def-health-check)
  (:import-from #:dag
                #:ordered-commits)
  (:export
   #:null-repo
   #:git-repo
   #:rev-parse
   #:merge-base
   #:current-commit
   #:current-branch
   #:cleanp
   #:git-message
   #:fetch-remote-branch
   #:author
   #:get-remote-url
   #:repo-dir
   #:rev-parse-local
   #:$
   #:extra-header
   #:debug-git-config
   #:credential-fill)
  (:local-nicknames (#:flags #:screenshotbot/sdk/flags)))
(in-package :screenshotbot/sdk/git)

(defun git-root (&key (directory (uiop:getcwd)) (errorp t))
  (declare (optimize (speed 0) (debug 3)))
  (cond
    ((path:-d (path:catdir directory ".git/"))
     directory)
    ((>= 1 (length (pathname-directory directory)))
     (if errorp
         (error "Could not find git root")
         nil))
    (t
     (git-root
      :directory (make-pathname
                  :directory (butlast (pathname-directory directory))
                  :defaults directory)
      :errorp errorp))))

(defclass git-repo ()
  ((link :initarg :link
         :accessor repo-link)
   (dir :initarg :dir
        :accessor repo-dir
        :initform (git-root))))

(defclass null-repo ()
  ((link :initarg :link
         :initform nil
         :accessor repo-link)))

(defmethod cleanp ((repo null-repo))
  t)

(defmethod repo-git-dir ((repo git-repo))
  (path:catdir (repo-dir repo) ".git/"))

(defmethod git-command ((repo git-repo))
  (list
   "git"
   "--git-dir"
   (namestring (repo-git-dir repo))))

(defmethod cleanp ((repo git-repo))
  (str:emptyp ($ (git-command repo) "status" "--porcelain")))

(defmethod git-message ((repo git-repo) &key (ref "HEAD"))
  "Shows the message of the given ref, which defaults to HEAD"
  ($ (git-command repo) "log" "-1" "--pretty=%B" "HEAD"))


(defmethod current-commit ((repo git-repo))
  ($ (git-command repo) "rev-parse" "HEAD"))

(defmethod current-commit ((repo null-repo))
  nil)

(defun $ (&rest args)
  (log:debug "Running command with args: ~S" args)
  (let* ((args (flatten args))
         (args (loop for arg in args
                    if (pathnamep arg)
                      collect (namestring arg)
                    else
                      collect arg))
         (out
           (uiop:run-program args
                             :output 'string
                             :error-output *error-output*)))
    (str:trim out)))

(defmethod current-branch ((repo git-repo))
  ($ (git-command repo) "rev-parse" "--abbrev-ref" "HEAD"))

(defmethod current-branch ((repo null-repo))
  nil)

(defun parse-raw-git-log (log)
  (let ((input (make-string-input-stream log))
        (output (make-string-output-stream)))
    (loop for line = (str:trim (read-line input nil nil))
          while line
          do
             (destructuring-bind (key &optional sha)
                 (str:split " " line :limit 2)
               (when (and
                      sha
                      (= (length sha) 40)
                      (cl-ppcre:scan "[a-f0-9]*" sha))
                 (cond
                   ((Equal "commit" key)
                    (format output "~%~a" sha))
                   ((equal "parent" key)
                    (format output " ~a" sha))))))
    (format nil "~a~%" (str:trim (get-output-stream-string output)))))

(defmethod read-graph ((repo git-repo))
  (dag:read-from-stream
   (make-string-input-stream
    (parse-raw-git-log
     ($ (git-command repo)
       "log" "--all"
       (when flags:*commit-limit*
         (format nil "--max-count=~a" flags:*commit-limit*))
       "--pretty=raw" "--no-color")))
   :format :text))

(defmethod merge-base ((repo git-repo) master-sha commit-sha)
  (unless master-sha
    (error "Main branch SHA missing"))
  (unless commit-sha
    (error "Commit SHA missing"))

  ($ (git-command repo) "merge-base" master-sha commit-sha))

(defmethod merge-base ((repo null-repo) master-sha commit-sha)
  nil)

(defmethod rev-parse ((repo git-repo) branch)
  (rev-parse-local
   repo
   (format nil "origin/~a" branch)))

(defmethod rev-parse-local ((repo git-repo) rev)
  "Technically should be called rev-parse, but for historical reasons
rev-parse compares against the remote branch :/"
  (handler-case
      ($ (git-command repo) "rev-parse" "-q" "--verify" rev)
    (error
      nil)))

(auto-restart:with-auto-restart (:retries 2)
  (defmethod %fetch-remote-branch ((repo git-repo) branch)
    (log:info "Running: git fetch origin ~a" branch)
    ($ (git-command repo) "fetch" "origin" branch)))

(defmethod fetch-remote-branch ((repo git-repo) branch)
  (unless (str:emptyp branch)
    (handler-case
        (%fetch-remote-branch repo branch)
      (error (e)
        (warn "Git fetch failed with ~a" e)))))

(defmethod origin (repo)
  "origin")

(defmethod fetch-remote-branch ((repo null-repo) branch)
  (values))

(defmethod rev-parse ((repo null-repo) branch)
  nil)

(defmethod author ((repo git-repo))
  ($ (git-command repo) "log" "--format=%ae" "-n" "1"))

(defmethod author ((repo null-repo))
  nil)

(defmethod get-remote-url ((repo null-repo))
  nil)

(defmethod get-remote-url ((repo git-repo))
  (ignore-errors
   ($ (git-command repo) "remote" "get-url" (origin repo))))

(def-health-check verify-git-is-present ()
  (uiop:run-program (list "git" "--help")))

(def-health-check verify-can-read-a-git-commit-graph ()
  (tmpdir:with-tmpdir (dir)
    (let ((*error-output* (make-string-output-stream)))
      ($ "git" "clone" "https://github.com/tdrhq/tiny-test-repo" dir))
    (let ((repo (make-instance 'git-repo :dir dir)))
      (let ((dag (read-graph repo)))
        (assert (> (length (dag:ordered-commits dag)) 0))))

    ;; On Windows, some Git files are read-only, which will make this test
    ;; fail
    #+windows
    (uiop:run-program (list "attrib" "-r" (format nil "~a\\*.*" (namestring dir)) "/s"))))

(defmethod debug-git-config ((repo git-repo))
  "Generates a string with enough information about what's in the
.git/config without revealing any secrets"
  (let ((config (path:catfile (repo-git-dir repo) "config")))
    (let ((lines (str:lines (uiop:read-file-string config))))
      (str:join #\Newline
       (loop for line in lines
             collect (str:substring 0 8 line))))))


(defmethod extra-header ((repo git-repo))
  (ignore-errors
   (loop for line in (str:lines
                      ($ (git-command repo)
                        "config" "--get-urlmatch"
                        "http.extraheader"
                        (get-remote-url repo)))
         collect
         (destructuring-bind (key value)
             (mapcar #'str:trim
                     (str:split ":" line))
           (cons (str:downcase key) value)))))

(defmethod credential-fill (url)
  "Call git credential fill to retrieve credentials for the given URL.
Returns a list of (username password) or NIL if no credentials found."
  (let* ((input (format nil "url=~a"
                        url)))
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program (list "git" "credential" "fill")
                          :input (make-string-input-stream input)
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (declare (ignore error-output))
      (when (zerop exit-code)
        (let ((lines (str:lines output))
              username
              password)
          (dolist (line lines)
            (let ((parts (str:split "=" line :limit 2)))
              (when (= 2 (length parts))
                (cond
                  ((equal "username" (first parts))
                   (setf username (second parts)))
                  ((equal "password" (first parts))
                   (setf password (second parts)))))))
          (when (and username password)
            (list username password)))))))

