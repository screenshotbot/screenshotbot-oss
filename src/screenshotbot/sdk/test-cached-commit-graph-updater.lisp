;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-cached-commit-graph-updater
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/commit-graph
                #:all-remote-refs
                #:remote-branch-sha
                #:update-commit-graph)
  (:import-from #:screenshotbot/sdk/cached-commit-graph-updater
                #:cached-commit-graph-updater
                #:hour-start
                #:cache-key
                #:cache-file
                #:read-cache-file
                #:write-cache-file
                #:prune-cache-dir
                #:default-commit-graph-cache-dir)
  (:import-from #:tmpdir
                #:with-tmpdir)
  (:import-from #:cl-mock
                #:if-called))
(in-package :screenshotbot/sdk/test-cached-commit-graph-updater)

(util/fiveam:def-suite)

(defclass fake-cached-commit-graph-updater (cached-commit-graph-updater)
  ((calls :initform 0
          :accessor calls)
   (refs :initarg :refs
         :initform '(("abcd" . "refs/heads/main")
                     ("ef01" . "refs/heads/feature"))
         :reader refs)))

(defmethod update-commit-graph ((self fake-cached-commit-graph-updater) repo branch
                                &key &allow-other-keys)
  (incf (calls self))
  (loop for (sha . ref) in (refs self)
        do (setf (gethash ref (all-remote-refs self)) sha))
  t)

(def-fixture cache (&key (build-url "https://ci.example.com/build/1")
                         (commit "aaaa"))
  (with-tmpdir (dir)
    (flet ((make (&rest args)
             ;; ARGS come first, so that they override the defaults
             (apply #'make-instance 'fake-cached-commit-graph-updater
                    (append args
                            (list :cache-dir dir
                                  :build-url build-url
                                  :commit commit)))))
      (&body))))

(test hour-start
  (let ((time (encode-universal-time 10 23 14 25 11 2026 0)))
    (is (eql (encode-universal-time 0 0 14 25 11 2026 0)
             (hour-start time)))
    (is (eql (hour-start time)
             (hour-start (hour-start time))))))

(test cache-key-depends-on-build-url-commit-and-hour
  (with-fixture cache ()
    (let ((now (encode-universal-time 10 23 14 25 11 2026 0)))
      (flet ((key (&key (build-url "https://ci.example.com/build/1")
                     (commit "aaaa")
                     (now now))
               (cache-key (make :build-url build-url :commit commit)
                          :now now)))
        (is (eql 64 (length (key))))
        (is (equal (key) (key :now (+ now 1000))))
        (is (not (equal (key) (key :now (+ now 3600)))))
        (is (not (equal (key) (key :commit "bbbb"))))
        (is (not (equal (key) (key :build-url "https://ci.example.com/build/2"))))))))

(test cache-miss-then-hit
  (with-fixture cache ()
    (let ((first (make)))
      (update-commit-graph first :repo "main")
      (is (eql 1 (calls first)))
      (is (probe-file (cache-file first))))
    (let ((second (make)))
      (update-commit-graph second :repo "main")
      (is (eql 0 (calls second)))
      (is (equal "abcd" (remote-branch-sha second "main")))
      (is (equal "ef01" (remote-branch-sha second "feature")))
      (is (eql 2 (hash-table-count (all-remote-refs second)))))))

(test different-commit-misses-the-cache
  (with-fixture cache ()
    (update-commit-graph (make) :repo "main")
    (let ((other (make :commit "bbbb")))
      (update-commit-graph other :repo "main")
      (is (eql 1 (calls other))))))

(test no-cache-without-build-url
  (with-fixture cache (:build-url nil)
    (update-commit-graph (make) :repo "main")
    (let ((second (make)))
      (update-commit-graph second :repo "main")
      (is (eql 1 (calls second))))
    (is (null (directory (merge-pathnames "*.*" dir))))))

(test empty-refs-are-not-cached
  (with-fixture cache ()
    (update-commit-graph (make :refs nil) :repo "main")
    (let ((second (make)))
      (update-commit-graph second :repo "main")
      (is (eql 1 (calls second))))))

(test write-cache-file-overwrites-and-leaves-no-tmp-files
  (with-fixture cache ()
    (let ((self (make))
          (file (merge-pathnames "foo.json" dir)))
      (setf (gethash "refs/heads/main" (all-remote-refs self)) "1111")
      (write-cache-file self file)
      (setf (gethash "refs/heads/main" (all-remote-refs self)) "2222")
      (write-cache-file self file)
      (is (equal (list (truename file))
                 (directory (merge-pathnames "*.*" dir))))
      (let ((other (make)))
        (is-true (read-cache-file other file))
        (is (equal "2222" (remote-branch-sha other "main")))))))

(defun write-file (file content)
  (with-open-file (stream file :direction :output
                               :if-exists :supersede
                               :external-format :utf-8)
    (write-string content stream)))

(test read-cache-file-happy-path
  (with-fixture cache ()
    (let ((self (make))
          (file (merge-pathnames "foo.json" dir)))
      (write-file file "{\"version\":1,\"refs\":[{\"name\":\"main\",\"sha\":\"abcd\"}]}")
      (is-true (read-cache-file self file))
      (is (equal "abcd" (remote-branch-sha self "main")))
      (is (eql 1 (hash-table-count (all-remote-refs self)))))))

(test read-cache-file-missing-or-other-version
  (with-fixture cache ()
    (let ((self (make))
          (file (merge-pathnames "foo.json" dir)))
      (setf (gethash "refs/heads/old" (all-remote-refs self)) "1111")
      (is-false (read-cache-file self (merge-pathnames "missing.json" dir)))
      (write-file file "{\"version\":2,\"refs\":[{\"name\":\"main\",\"sha\":\"abcd\"}]}")
      (is-false (read-cache-file self file))
      (is (equal "1111" (remote-branch-sha self "old")))
      (is (eql 1 (hash-table-count (all-remote-refs self)))))))

(test read-cache-file-signals-on-corrupt-files
  (with-fixture cache ()
    (let ((self (make))
          (file (merge-pathnames "foo.json" dir)))
      (setf (gethash "refs/heads/old" (all-remote-refs self)) "1111")
      (dolist (content (list "garbage"
                             "{\"version\":1,\"refs\":[{\"name\":\"main\"}]}"
                             "{\"version\":1,\"refs\":[{\"name\":\"\",\"sha\":\"abcd\"}]}"))
        (write-file file content)
        (signals error
          (read-cache-file self file)))
      (is (equal "1111" (remote-branch-sha self "old")))
      (is (eql 1 (hash-table-count (all-remote-refs self)))))))

(test corrupt-cache-file-is-a-miss
  (with-fixture cache ()
    (let ((self (make)))
      (write-file (cache-file self) "{\"version\":1,")
      (finishes (update-commit-graph self :repo "main"))
      (is (eql 1 (calls self)))
      ;; And it got replaced with a good one
      (let ((second (make)))
        (update-commit-graph second :repo "main")
        (is (eql 0 (calls second)))))))

(defun cache-files (dir)
  (directory (merge-pathnames "*.*" dir)))

(test only-branch-refs-are-cached
  (with-fixture cache ()
    (let ((first (make :refs '(("abcd" . "refs/heads/main")
                               ("1234" . "refs/tags/v1")
                               ("5678" . "refs/pull/1/head")))))
      (update-commit-graph first :repo "main")
      (is (equal "{\"version\":1,\"refs\":[{\"name\":\"main\",\"sha\":\"abcd\"}]}"
                 (uiop:read-file-string (cache-file first)))))
    (let ((second (make)))
      (update-commit-graph second :repo "main")
      (is (eql 0 (calls second)))
      (is (equal "abcd" (remote-branch-sha second "main")))
      (is (eql 1 (hash-table-count (all-remote-refs second)))))))

(test no-cache-without-branch-refs
  (with-fixture cache ()
    (update-commit-graph (make :refs '(("1234" . "refs/tags/v1"))) :repo "main")
    (is (null (cache-files dir)))))

(test bad-default-cache-dir-is-a-miss
  (with-fixture cache ()
    (cl-mock:with-mocks ()
      (if-called 'default-commit-graph-cache-dir
                 (lambda ()
                   (error "Expected an absolute pathname")))
      ;; Creating the object must not touch the cache dir
      (let ((self (make :cache-dir nil)))
        (finishes (update-commit-graph self :repo "main"))
        (is (eql 1 (calls self)))
        (is (equal "abcd" (remote-branch-sha self "main")))))))

(test undecodable-cache-file-is-a-miss
  (with-fixture cache ()
    (let* ((self (make))
           (file (cache-file self)))
      (with-open-file (stream file :direction :output
                                   :element-type '(unsigned-byte 8))
        (write-sequence #(#xff #xfe #xfd 10) stream))
      (finishes (update-commit-graph self :repo "main"))
      (is (eql 1 (calls self))))))

(test directory-at-cache-file-path-is-a-miss
  (with-fixture cache ()
    (let ((self (make)))
      (ensure-directories-exist
       (uiop:ensure-directory-pathname (cache-file self)))
      (finishes (update-commit-graph self :repo "main"))
      (is (eql 1 (calls self)))
      (is (equal "abcd" (remote-branch-sha self "main"))))))

(test prune-cache-dir-deletes-only-old-files
  (with-fixture cache ()
    (let ((self (make)))
      (update-commit-graph self :repo "main")
      (is (eql 1 (length (cache-files dir))))
      (prune-cache-dir self)
      (is (eql 1 (length (cache-files dir))))
      (prune-cache-dir self :now (+ (get-universal-time) (* 3 3600)))
      (is (null (cache-files dir))))))
