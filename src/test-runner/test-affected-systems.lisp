(defpackage :test-runner/test-affected-systems
  (:use #:cl
        #:fiveam)
  (:import-from #:test-runner/affected-systems
                #:changed-files
                #:needs-rebuild-all
                #:filter-affected-systems)
  (:import-from #:tmpdir
                #:with-tmpdir)
  (:local-nicknames (#:a #:alexandria)))
(in-package :test-runner/test-affected-systems)

(util/fiveam:def-suite)

(defun git (dir &rest args)
  (uiop:run-program (list* "git" "-c" "user.name=Test" "-c" "user.email=test@example.com"
                           args)
                    :directory dir
                    :output :string))

(defun write-file (dir name content)
  (let ((file (path:catfile dir name)))
    (ensure-directories-exist file)
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (write-string content stream))))

(defun commit (dir)
  (git dir "add" "-A")
  (git dir "commit" "-q" "-m" "commit")
  (string-trim '(#\Newline) (git dir "rev-parse" "HEAD")))

(test changed-files-between-commits
  (with-tmpdir (dir)
    (git dir "init" "-q")
    (write-file dir "a.lisp" "a")
    (write-file dir "b.lisp" "b")
    (write-file dir "sub/c.lisp" "c")
    (let ((old (commit dir)))
      (write-file dir "b.lisp" "b2")
      (write-file dir "sub/d.lisp" "d")
      (commit dir)
      (is (equal (list "b.lisp" "sub/d.lisp")
                 (changed-files old :root dir)))
      (is (equal nil
                 (changed-files old :new-commit old :root dir))))))

(test renames-report-the-old-path
  (with-tmpdir (dir)
    (git dir "init" "-q")
    (write-file dir "a.lisp" "some reasonably long content so git detects the rename")
    (let ((old (commit dir)))
      (rename-file (path:catfile dir "a.lisp") (path:catfile dir "b.lisp"))
      (commit dir)
      (is (equal (list "a.lisp" "b.lisp")
                 (changed-files old :root dir))))))

(test needs-rebuild-all
  (with-tmpdir (dir)
    (write-file dir "foo.lisp" "")
    (write-file dir "foo.asd" "")
    (write-file dir "Makefile" "")
    (write-file dir "scripts/init.lisp" "")
    (is-false (needs-rebuild-all (list "foo.lisp") dir))
    (is-true (needs-rebuild-all (list "foo.lisp" "foo.asd") dir))
    (is-true (needs-rebuild-all (list "deleted.lisp") dir))
    (is-true (needs-rebuild-all (list "Makefile") dir))
    (is-true (needs-rebuild-all (list "scripts/init.lisp") dir))))

(def-fixture fake-systems ()
  (with-tmpdir (dir)
    (write-file dir "a.lisp" "")
    (write-file dir "b.lisp" "")
    (write-file dir "c.lisp" "")
    (write-file dir "tr-fake.asd"
                "(defsystem :tr-fake :components ((:file \"a\")))
(defsystem :tr-fake/b :depends-on (:tr-fake) :components ((:file \"b\")))
(defsystem :tr-fake/c :components ((:file \"c\")))")
    (asdf:load-asd (path:catfile dir "tr-fake.asd"))
    (unwind-protect
         (&body)
      (mapc #'asdf:clear-system (list "tr-fake" "tr-fake/b" "tr-fake/c")))))

(test filter-affected-systems
  (with-fixture fake-systems ()
    (let ((systems (list "tr-fake/b" "tr-fake/c")))
      (is (equal (list "tr-fake/b")
                 (filter-affected-systems systems (list "b.lisp") dir)))
      (is (equal (list "tr-fake/b")
                 (filter-affected-systems systems (list "a.lisp") dir)))
      (is (equal (list "tr-fake/c")
                 (filter-affected-systems systems (list "c.lisp") dir)))
      (is (equal nil
                 (filter-affected-systems systems nil dir))))))

(test filter-affected-systems-through-a-symlink
  (with-fixture fake-systems ()
    (let ((link (format nil "~a-link" (string-right-trim "/" (namestring dir)))))
      (uiop:run-program (list "ln" "-s" (string-right-trim "/" (namestring dir)) link))
      (unwind-protect
           (is (equal (list "tr-fake/b")
                      (filter-affected-systems (list "tr-fake/b")
                                               (list "b.lisp")
                                               (pathname (format nil "~a/" link)))))
        (delete-file link)))))
