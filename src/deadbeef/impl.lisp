(uiop:define-package :deadbeef/impl
    (:use #:cl)
  (:export
   #:*cache-dir*
   #:register-external
   #:prepare-externals))
(in-package :deadbeef/impl)

(defvar *externals* nil)

(defvar *cache-dir* nil)

(defvar *whitespaces* '(#\Space #\Newline #\Backspace #\Tab
                        #\Linefeed #\Page #\Return #\Rubout))

(defun trim (value)
  (string-trim *whitespaces* value))

(defun register-external (repo commit)
  (pushnew (cons repo commit)
           *externals*
           :test #'equal))

(defun run-program-with-errors (cmd)
  (multiple-value-bind (out err ret)
      (uiop:run-program cmd
                        :output 'string
                        :ignore-error-status t
                        :error-output 'string)
    (unless (eql 0 ret)
      (error "Bash command `~a` failed: ~%stdout: ~a~%~% stderr:~%~A~%"
             cmd
             out err))
    (trim out)))

(defun catdir (x y)
  (let ((x (pathname x))
        (y (pathname y)))
    (assert (eql :relative (car (pathname-directory y))))
    (make-pathname
     :directory (cons
                 (car (pathname-directory x))
                 (append (cdr (pathname-directory x)) (cdr (pathname-directory y))))
     :defaults x)))


;; taken from cl-fad, with slight modifications
(defun directory-exists-p (pathspec)
  "Checks whether the file named by the pathname designator PATHSPEC
exists and if it is a directory.  Returns its truename if this is the
case, NIL otherwise.  The truename is returned in directory form as if
by PATHNAME-AS-DIRECTORY."
  #+:allegro
  (and (excl:probe-directory pathspec)
       (truename pathspec))
  #+:lispworks
  (and (lw:file-directory-p pathspec)
       (truename pathspec))
  #-(or :allegro :lispworks)
  (let ((result (file-exists-p pathspec)))
    (and result
         (directory-pathname-p result))))


(defun prepare-git-repo (repo commit cache-dir)
  (let ((git-dir (catdir cache-dir ".git/")))
   (cond
     ((directory-exists-p git-dir)
      (run-program-with-errors (list
                                "git" "--work-tree" (namestring cache-dir)
                                "--git-dir" (namestring git-dir)
                                "fetch"
                                repo)))
     (t
      (run-program-with-errors (list
                                "git" "clone"
                                repo
                                (namestring  cache-dir)))))

    ;; checkout the right commit
    (run-program-with-errors (list
                              "git" "--work-tree" (namestring cache-dir)
                              "--git-dir" (namestring git-dir)
                              "checkout"
                              commit))))

(defun name-from-repo-name (repo-name)
  (let ((pos (position #\/ repo-name :from-end t)))
    (subseq repo-name (+ 1 pos))))

(defun prepare-externals (cache-dir)
  (setf *cache-dir* cache-dir)
  (loop for (repo . commit) in *externals*
        do
           (let* ((name (name-from-repo-name repo))
                  (cache-dir (catdir *cache-dir* (format nil "~a/" name))))
             (pushnew cache-dir
                      asdf:*central-registry*
                      :test 'equal)
             (prepare-git-repo repo commit cache-dir))))
