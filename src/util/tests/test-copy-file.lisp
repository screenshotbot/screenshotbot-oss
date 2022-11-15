;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-copy-file
  (:use #:cl
        #:fiveam)
  (:import-from #:util/copy-file
                #:copy-file-fast)
  (:import-from #:tmpdir
                #:with-tmpdir)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/tests/test-copy-file)


(util/fiveam:def-suite)

(defun write-file (src text)
  (with-open-file (src src
                       :direction :output)
    (format src text)))

(def-fixture state ()
  (with-tmpdir (dir)
    (let ((src (path:catfile dir "foo.txt"))
          (dest (path:catfile dir "bar.txt")))
      (write-file src "hello world")
      (&body))))

#+lispworks
(defun inode (file)
  (sys:file-stat-inode
   (sys:get-file-stat file)))

(test simple-copy-file
  (with-fixture state ()
    (copy-file-fast src dest)
    (is (equal "hello world"
               (uiop:read-file-string dest)))
    #+lispworks
    (is (eql (inode src) (inode dest)))))

(test if-dest-exists-we-fail
  ;; This guards against the possibility that both src and dest point
  ;; to the same file. If that happens, then we'll rewrite the file,
  ;; and during the rewrite the file might be in a bad state.
  (with-fixture state ()
    (write-file dest "foo")
    (signals error
      (copy-file-fast src dest))
    (is (equal "foo" (uiop:read-file-string dest)))))


;; This test depends on a very specific directory layout, so it's
;; disable for now.
#+nil
(test copy-file-to-temp
  (uiop:with-temporary-file (:pathname p :directory "/data/arnold/")
    (delete-file p)
    (finishes
     (copy-file-fast (asdf:system-relative-pathname :util "copy-file.lisp")
                     p))))
