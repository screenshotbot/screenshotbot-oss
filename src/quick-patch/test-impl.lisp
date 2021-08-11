;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :quick-patch/test-impl
    (:use #:cl
          #:fiveam
          #:alexandria)
  (:import-from #:quick-patch/impl
                #:catdir
                #:name-from-repo-name
                #:run-program-with-errors
                #:prepare-git-repo
                #:checkout-all
                #:*cache-dir*
                #:*externals*
                #:register)
  (:import-from #:asdf
                #:*central-registry*))
(in-package :quick-patch/test-impl)

(def-suite* :quick-patch/test-impl)

(defun add-external-repo ()
  (register "https://github.com/m0cchi/cl-slack"
                     "019ecb3"))

(test name-from-repo-name
  (is (equal "foo" (name-from-repo-name "https://github.com/xyz/foo"))))

(test catdir
  (is (equal #P "/foo/bar/" (catdir "/foo/" "bar/"))))

(test test-registry
  (let ((*externals* nil))
    (add-external-repo)
    (is (equal *externals*
               (list (list
                      "https://github.com/m0cchi/cl-slack"
                      "019ecb3"))))))

(test load-externals
  (cl-mock:with-mocks ()
    (tmpdir:with-tmpdir (dir)
      (let ((*cache-dir* nil)
            (*externals* nil)
            (*central-registry* *central-registry*))
        (add-external-repo)
        (let ((expected-dir (path:catdir dir "cl-slack/")))
          (cl-mock:if-called 'prepare-git-repo
                              (lambda (repo commit cache-dir)
                                (is (equal repo "https://github.com/m0cchi/cl-slack"))
                                (is (equal commit "019ecb3"))))
          (checkout-all dir)
          (is (member expected-dir
                      *central-registry*
                      :test 'equal)))
        (pass)))))


(test prepare-git-repo-integration-test
  (tmpdir:with-tmpdir (dir)
    (flet ((bash (x)
             (run-program-with-errors (list "bash" "-c"
                                            (format nil "cd ~a && ~a"
                                                    (namestring dir)
                                                    x)))))
      (signals error
        (bash "false"))
      (bash "true")
      (bash "which git")
      (bash "mkdir source")
      (bash "cd source && git init .")
      (bash "cd source && git config user.email foo@tdrhq.com")
      (bash "cd source && git config user.name 'Foo Bar'")
      (bash "cd source && echo hello > file.txt")
      (bash "cd source && git add file.txt")
      (bash "cd source && git commit -a -m first-commit")
      (bash "cd source && echo hello2 > file.txt")
      (bash "cd source && git commit -a -m second-commit")
      (let ((first-commit (bash "cd source && git rev-parse HEAD^"))
            (second-commit (bash "cd source && git rev-parse HEAD")))
        ;; let's use this repo to make our new clone
        (finishes
         (prepare-git-repo (namestring (path:catdir dir "source/"))
                           first-commit
                           (path:catdir dir "dest/")))



        (is (equal first-commit (bash "cd dest && git rev-parse HEAD")))
        (is (equal "hello" (str:trim (uiop:read-file-string (path:catfile dir "dest/file.txt")))))

        ;; can I clone to the same directory again?
        (finishes
         (prepare-git-repo (namestring (path:catdir dir "source/"))
                           first-commit
                           (path:catdir dir "dest/")))

        ;; can I switch the commit?
        (finishes
          (prepare-git-repo (namestring (path:catdir dir "source/"))
                            second-commit
                            (path:catdir dir "dest/")))

        (is (equal "hello2" (str:trim (uiop:read-file-string (path:catfile dir "dest/file.txt")))))))))
