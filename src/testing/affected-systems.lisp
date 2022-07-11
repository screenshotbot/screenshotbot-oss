(defpackage :testing/affected-systems
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:filter-affected-systems))
(in-package :testing/affected-systems)

(defun action-deps (action)
  (destructuring-bind (op . component) action
    (asdf::direct-dependencies op component)))

(defparameter *bad-files*
  (list "jenkins.lisp"
        "scripts/init.lisp"
        "scripts/prepare-image.lisp"
        "Makefile"))

(defun needs-rebuild-all (files)
  (loop for f in files
        if (or (equal "asd" (pathname-type f))
               (not (path:-e f))
               (member f *bad-files* :test #'equal))
          return t))

(defun filter-affected-systems (systems files
                                &optional
                                  (root (asdf:system-relative-pathname :web.all "../")))
  "Keep only the systems in `systems` that are going to be affected by
  the changed `files`. Files are relative to the git root, and may
  point to a non-existant file, in which case we consider the file
  to be deleted."
  (let ((files (loop for x in files collect (path:catfile root x)))
        (roots (loop for x in systems
                     collect `(asdf:load-op . ,x)))
        (cache (make-hash-table :test #'equal)))
    (cond
      ((needs-rebuild-all files)
       (log:info "We'll be rebuilding all the input targets")
       (mapcar #'cdr roots))
      (t

       (dolist (f files)
         (setf (gethash f cache) t))
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
                                                             (gethash (asdf:component-pathname component) cache))))))
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
