;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :quick-patch/test-impl
    (:use #:cl
          #:fiveam
          #:fiveam-matchers)
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
                      "019ecb3"
                      (list (make-pathname :directory `(:relative)))))))))

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
          (assert-that (mapcar #'namestring *central-registry*)
                       (has-item
                        (matches-regex ".*cl-slack/$"))))
        (pass)))))


(test prepare-git-repo-integration-test
  (tmpdir:with-tmpdir (dir)
    (let ((source (ensure-directories-exist (path:catdir dir "source/")))
          (dest (path:catdir dir "dest/")))
      (flet ((bash (x)
               (run-program-with-errors (str:split " " x)  :directory source))
             (bash-dest (x)
               (run-program-with-errors (str:split " " x) :directory dest)
        (bash "git init .")
        (bash "git config user.email foo@tdrhq.com")
        (bash "git config user.name FooBar")
        (with-open-file (file.txt (path:catfile source file.txt)
                                  :direction :output)
          (write-string "hello" file.txt))
        (bash "git add file.txt")
        (bash "git commit -a -m first-commit")
        (with-open-file (file.txt (path:catfile source file.txt)
                                  :direction :output
                                  :if-exists :supersede)
          (write-string "hello2" file.txt))
        (bash "git commit -a -m second-commit")
        (let ((first-commit (bash "git rev-parse HEAD^"))
              (second-commit (bash "git rev-parse HEAD")))
          ;; let's use this repo to make our new clone
          (finishes
            (prepare-git-repo (namestring source)
                              first-commit
                              dest))

        (is (equal first-commit (bash-dest "git rev-parse HEAD")))
        (is (equal "hello" (str:trim (uiop:read-file-string (path:catfile dir "dest/file.txt")))))

        ;; can I clone to the same directory again?
        (finishes
          (prepare-git-repo (namestring source)
                            first-commit
                            dest))

        ;; can I switch the commit?
        (finishes
          (prepare-git-repo (namestring source)
                            second-commit
                            dest))

        (is (equal "hello2" (str:trim (uiop:read-file-string (path:catfile dir "dest/file.txt"))))))))))))
