(defpackage :test-runner/affected-systems
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:filter-affected-systems
   #:changed-files))
(in-package :test-runner/affected-systems)

(defun action-deps (action)
  (destructuring-bind (op . component) action
    (asdf::direct-dependencies op component)))

(defparameter *bad-files*
  (list "jenkins.lisp"
        "scripts/init.lisp"
        "scripts/prepare-image.lisp"
        "Makefile"))

(defun needs-rebuild-all (files root)
  "`files` are relative to `root`."
  (loop for f in files
        if (or (equal "asd" (pathname-type f))
               (not (path:-e (path:catfile root f)))
               (member f *bad-files* :test #'equal))
          return t))

;; --no-renames makes a rename show up as a delete plus an add, so the
;; old path is reported as missing and we fall back to rebuilding
;; everything, same as for any other deleted file.
(defun changed-files (old-commit
                      &key
                        (new-commit "HEAD")
                        (root (asdf:system-relative-pathname :web.all "../")))
  "The files that differ between `old-commit` and `new-commit`, relative
  to the git root, in the form `filter-affected-systems` expects."
  (uiop:run-program (list "git" "diff" "--name-only" "--no-renames"
                          old-commit new-commit "--")
                    :directory root
                    :output :lines
                    :error-output *error-output*))

;; Compare namestrings: EQUAL on pathnames also compares host and
;; device, which might be NIL or :UNSPECIFIC depending on how the
;; pathname was constructed. We also take the truename of the changed
;; files, since `root` might go through a symlink that ASDF has
;; already resolved in the component pathnames (e.g. /var ->
;; /private/var on macOS). Doing the same for every component would be
;; slow, and isn't needed.

(defun filter-affected-systems (systems files
                                &optional
                                  (root (asdf:system-relative-pathname :web.all "../")))
  "Keep only the systems in `systems` that are going to be affected by
  the changed `files`. Files are relative to the git root, and may
  point to a non-existant file, in which case we consider the file
  to be deleted."
  (let ((relative-files files)
        (files (loop for x in files collect (path:catfile root x)))
        (roots (loop for x in systems
                     collect `(asdf:load-op . ,x)))
        (cache (make-hash-table :test #'equal)))
    (cond
      ((needs-rebuild-all relative-files root)
       (log:info "We'll be rebuilding all the input targets")
       (mapcar #'cdr roots))
      (t

       (dolist (f files)
         (setf (gethash (namestring (truename f)) cache) t))
       (let ((results (graphs:dfs roots
                                  :children #'action-deps
                                  :node-test #'equal
                                  :after-hook (lambda (action results)
                                                (let ((res
                                                        (or
                                                         (some #'identity results)
                                                         (destructuring-bind (op . component) action
                                                           (declare (ignore op))
                                                           (when (typep component 'asdf:file-component)
                                                             (gethash (namestring (asdf:component-pathname component)) cache))))))
                                                  #+nil
                                                  (log:info
                                                   "Looking at ~s and got ~s (results: ~s for ~s)"
                                                   action res results (action-deps action))
                                                  res)))))
         (values
          (loop for x in roots
                for y in results
                if y
                  collect (cdr x))
          results))))))
