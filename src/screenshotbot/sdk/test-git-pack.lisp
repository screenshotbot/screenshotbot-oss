;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-git-pack
  (:use #:cl
        #:fiveam)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/strings
                #:starts-with)
  (:import-from #:screenshotbot/sdk/git-pack
                #:http-upload-pack
                #:%get-auth
                #:has-authorization-header-p
                #:upload-pack-error
                #:remove-auth-from-uri
                #:read-netrc
                #:supported-remote-repo-p
                #:read-commits
                #:read-packfile
                #:read-packfile-header
                #:read-packfile-entry
                #:counting-input-stream
                #:counting-position
                #:+obj-commit+
                #:+obj-ofs-delta+
                #:+obj-ref-delta+
                #:apply-delta
                #:read-ofs-delta-offset
                #:parse-parents
                #:make-upload-pack-command)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:alexandria
                #:assoc-value)
  (:local-nicknames (#:test-git #:screenshotbot/sdk/test-git)
                    (#:git #:screenshotbot/sdk/git)))
(in-package :screenshotbot/sdk/test-git-pack)

(util/fiveam:def-suite)

(test parse-remote-git-pack
  (assert-that
   (str:join " " (make-upload-pack-command "/tmp/foo/bar.git"))
   (starts-with "/usr/bin/env git-upload-pack"))
  (assert-that
   (str:join " " (make-upload-pack-command "user@host:tmp/foo.git"))
   (starts-with "/usr/bin/env ssh user@host git-upload-pack "))  
  (assert-that
   (str:join " " (make-upload-pack-command "ssh://user@host/tmp/foo.git"))
   (starts-with "/usr/bin/env ssh user@host git-upload-pack "))
  (assert-that
   (str:join " "(make-upload-pack-command "user@foo.host.com:tmp/foo.git"))
   (starts-with "/usr/bin/env ssh user@foo.host.com git-upload-pack "))  
  (assert-that
   (str:join " " (make-upload-pack-command "ssh://user@foo.host.com/tmp/foo.git"))
   (starts-with "/usr/bin/env ssh user@foo.host.com git-upload-pack "))
  (assert-that
   (str:join " " (make-upload-pack-command "ssh://user@foo.host.com/tmp/foo.git"))
   (starts-with "/usr/bin/env ssh user@foo.host.com git-upload-pack '/tmp/foo.git'"))
  (assert-that
   (str:join " " (make-upload-pack-command "ssh://git@phabricator.tdrhq.com:2222/source/web.git"))
   (starts-with "/usr/bin/env ssh git@phabricator.tdrhq.com -p 2222 git-upload-pack '/source/web.git'")))

(test parse-parents
  (is (equal
       (list
        "334730e618c03bf2989eda474fc600953e000ec0"
        "9c2a4ee0b041d286a4d79ab42d15c68b87d0cccd")
       (parse-parents
        "tree f10b145f8bf50b0503c8becc6c3a202b2bdd4308
parent 334730e618c03bf2989eda474fc600953e000ec0
parent 9c2a4ee0b041d286a4d79ab42d15c68b87d0cccd
author doc001 <wenffie@gmail.com> 1685501589 +0800
committer GitHub <noreply@github.com> 1685501589 +0800
gpgsig -----BEGIN PGP SIGNATURE-----
 
 wsBcBAABCAAQBQJkdraVCRBK7hj4Ov3rIwAAF5QIAFPvRp2NctvDOeNr45tqgjnT
 nuO7p1FUdsa1YExbP8GaJ+iuf+mJnTnUsxFXkTH95u/CUo2BLEEJHS/pe/2kR7v9
 XYW10/JQOD9nst7xKDM6PBDKKOjc2ljmQOeSroIDid0yQ5yedo7ENW2Vd8rJWmMT
 b0oVuEkb0nmU8FygktAl8YHB81oKYKo3xxeZQMtPzCY39geIjn27J9jrd1wqC+OA
 X1oVEU6bJV4U5r6c2jReh5txmuZAyyODSojsssztM/EH1+g8ivBYM2Qypcd5lRqS
 Qv71S/iil2Ta5oKq/F9Wj6Bfd8L3+skZ9JIyDhaiT1qW1Z+J7tzkjqKJLh3oSNw=
 =M9w9
 -----END PGP SIGNATURE-----
 

Merge pull request #401 from nanblpc/set-idle-when-block-timeout

set st to idle when block timeout"))))

(test simple-repo-get-commits-integration
  (test-git:with-git-repo (repo :dir dir)
    (test-git:make-commit repo "foo")
    (test-git:make-commit repo "bar")
    (test-git:enable-server-features repo)
    (let ((commits
            (read-commits (namestring dir) :wants (list "main" "master"))))
      (assert-that
       commits
       (has-length 2)))))

(test simple-repo-get-commits-with-one-have
  (test-git:with-git-repo (repo :dir dir)
    (test-git:make-commit repo "foo")
    (test-git:make-commit repo "bar")
    (test-git:enable-server-features repo)
    (let ((commits
            (read-commits (namestring dir) :wants (list "main" "master")
                          :haves (list (git:current-commit repo)))))
      (assert-that
       commits
       (has-length 0)))))

(test simple-repo-get-commits-with-duplicate-haves
  (test-git:with-git-repo (repo :dir dir)
    (test-git:make-commit repo "foo")
    (test-git:make-commit repo "bar")
    (test-git:enable-server-features repo)
    (let ((commits
            (read-commits (namestring dir) :wants (list "main" "master")
                                           :haves (list (git:current-commit repo)
                                                        (git:current-commit repo)))))
      (assert-that
       commits
       (has-length 0)))))

(test simple-repo-get-commits-with-haves-one-after-the-other
  (test-git:with-git-repo (repo :dir dir)
    (test-git:make-commit repo "foo")
    (test-git:make-commit repo "bar")
    (test-git:enable-server-features repo)
    (let ((commits
            (read-commits (namestring dir) :wants (list "main" "master")
                                           :haves (list (git:current-commit repo)
                                                        (git:rev-parse-local repo "HEAD^")))))
      (assert-that
       commits
       (has-length 0)))))

(test simple-repo-get-commits-that-does-not-exist
  (test-git:with-git-repo (repo :dir dir)
    (test-git:make-commit repo "foo")
    (test-git:make-commit repo "bar")
    (test-git:enable-server-features repo)
    (let ((commits
            (read-commits (namestring dir) :wants (list "main" "master")
                                           :haves (list "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))))
      (assert-that
       commits
       (has-length 2)))))



(test azure-IS-supported
  "Used to say `is not supported`"
  (is-true (supported-remote-repo-p "git@ssh.dev.azure.com:v3/testsbot/fast-example/fast-example"))
  (is-true (supported-remote-repo-p "git@github.com:fast-example/fast-example")))

(test netrc
  (uiop:with-temporary-file (:pathname p :stream s :direction :output)
    (format s "machine github.com~%login foo  ~%  password bar_car~%~%")
    (finish-output s)
    (is (equal
         (list "foo" "bar_car")
         (read-netrc "github.com" :pathname p))))

  (uiop:with-temporary-file (:pathname p :stream s :direction :output)
    (format s "  #hello ~%machine github.com~%login foo  ~%  password bar_car~%~%")
    (finish-output s)
    (is (equal
         (list "foo" "bar_car")
         (read-netrc "github.com" :pathname p))))

  (uiop:with-temporary-file (:pathname p :stream s :direction :output)
    (format s "machine github.com~%login foo  ~%# login bar~%  password bar_car~%~%")
    (finish-output s)
    (is (equal
         (list "foo" "bar_car")
         (read-netrc "github.com" :pathname p))))

  (uiop:with-temporary-file (:pathname p :stream s :direction :output)
    (format s "machine github.comx~%login foo  ~%# login bar~%  password bar_car~%~%")
    (finish-output s)
    (is (equal
         nil
         (read-netrc "github.com" :pathname p))))

  (tmpdir:with-tmpdir (dir)
    (read-netrc "github.com" :pathname (path:catfile dir ".netrc"))))

(test remove-auth-from-uri
  (is
   (equal
    (list "https://gitlab.com/my-group/my-repo.git"
          (list "gitlab-ci-token" "foo"))
    (multiple-value-list
     (remove-auth-from-uri "https://gitlab-ci-token:foo@gitlab.com/my-group/my-repo.git")))))

(test remove-auth-from-uri-when-theres-no-auth
  (is
   (equal
    (list "https://gitlab.com/my-group/my-repo.git"
          nil)
    (multiple-value-list
     (remove-auth-from-uri "https://gitlab.com/my-group/my-repo.git")))))

(test not-our-ref-error
  "Reproduces the 'fatal: git upload-pack: not our ref' error.
This happens when requesting a commit SHA that doesn't exist in the repository,
typically because it was removed via force push or history rewrite."
  (test-git:with-git-repo (repo :dir dir)
    (test-git:make-commit repo "foo")
    (test-git:make-commit repo "bar")
    (test-git:enable-server-features repo)
    (signals upload-pack-error
      (read-commits (namestring dir)
                    :wants (lambda (refs)
                             ;; Return a fake SHA that doesn't exist
                             (list "458a81c522f6d7325c9d893624c926eaa8ed2cc0"))
                    :depth 30))))

(test git-credential-fill-basic
  "Test that git-credential-fill calls git credential fill with proper input"
  ;; This test checks the basic functionality without actually calling git
  ;; In a real environment with git credential configured, this would return credentials
  (let ((result (git:credential-fill "https://github.com/org/repo.git")))
    ;; Result should either be nil (no credentials configured) or a list of (username password)
    (is (or (null result)
            (and (listp result)
                 (= 2 (length result))
                 (stringp (first result))
                 (stringp (second result)))))))

(test has-authorization-header-p
  (is-true
   (has-authorization-header-p
    `((:authorization "sdfdsfd"))))
  (is-false
   (has-authorization-header-p
    `((:fake-key "sdfdsfd")))))

(test %get-auth
  (cl-mock:with-mocks ()
    (cl-mock:if-called 'read-netrc
                       (lambda (&rest args)
                         (error "should not have been called")))
    (cl-mock:if-called 'git:credential-fill
                       (lambda (&rest args)
                         (error "should not have been called")))
   (let ((upload-pack (make-instance 'http-upload-pack
                                     :extra-headers `(("Authorization" . "Bearer foo"))
                                     :repo "https://arnold:bar@github.com/tdrhq/fast-example.git")))
     (is (equal nil (%get-auth upload-pack))))
    (let ((upload-pack (make-instance 'http-upload-pack
                                      :extra-headers `(("X-car" . "foo"))
                                      :repo "https://arnold:bar@github.com/tdrhq/fast-example.git")))
      (is (equal '("arnold" "bar") (%get-auth upload-pack))))))

(test %get-auth-does-not-call-netrc-at-all
  (cl-mock:with-mocks ()
    (cl-mock:if-called 'read-netrc
                       (lambda (&rest args)
                         (error "should not have been called")))
    (cl-mock:if-called 'git:credential-fill
                       (lambda (&rest args)
                         (error "should not have been called")))
    (let ((upload-pack (make-instance 'http-upload-pack
                                      :extra-headers `(("Authorization" . "Bearer foo"))
                                      :repo "https://github.com/tdrhq/fast-example.git")))
      (is (equal nil (%get-auth upload-pack))))))
;;; --- Deltified commits ---

(test apply-delta
  (let ((base (flex:string-to-octets "hello world"))
        (delta (concatenate '(vector (unsigned-byte 8))
                            ;; base size, result size
                            #(11 11)
                            ;; copy 6 bytes from offset 0
                            #(#x90 6)
                            ;; insert 5 bytes
                            #(5)
                            (flex:string-to-octets "there"))))
    (is (equal "hello there"
               (flex:octets-to-string (apply-delta base delta)))))
  (let ((base (flex:string-to-octets "hello world"))
        (delta (concatenate '(vector (unsigned-byte 8))
                            #(11 5)
                            ;; copy 5 bytes from offset 6
                            #(#x91 6 5))))
    (is (equal "world"
               (flex:octets-to-string (apply-delta base delta))))))

(test apply-delta-rejects-wrong-base-size
  (signals error
    (apply-delta (flex:string-to-octets "hello")
                 (coerce #(11 1 1 65) '(vector (unsigned-byte 8))))))

(test read-ofs-delta-offset
  (flet ((decode (bytes)
           (flex:with-input-from-sequence (s (coerce bytes '(vector (unsigned-byte 8))))
             (read-ofs-delta-offset s))))
    (is (eql 5 (decode #(5))))
    (is (eql 127 (decode #(#x7f))))
    ;; Unlike the entry header, each continuation adds one before
    ;; shifting, so #x80 #x00 is 128 and not 0.
    (is (eql 128 (decode #(#x80 0))))
    (is (eql (+ (ash (1+ #x11) 7) #x2e) (decode #(#x91 #x2e))))))

(defun deltified-commits (repo)
  "Returns (SHA . DEPTH) for each commit that is stored as a delta in
REPO's packs, where DEPTH is the length of its delta chain."
  (let ((output (test-git::run-in-dir
                 repo
                 "git verify-pack -v .git/objects/pack/*.idx"
                 :output :string)))
    (loop for line in (str:lines output)
          for parts = (str:split " " (str:collapse-whitespaces line))
          ;; sha type size size-in-pack offset depth base-sha
          if (and (equal "commit" (second parts))
                  (eql 7 (length parts)))
            collect (cons (first parts)
                          (parse-integer (sixth parts))))))

(defun all-commits (repo)
  (str:lines
   (str:trim
    (test-git::run-in-dir repo "git rev-list --all" :output :string))))

(def-fixture deltified-repo ()
  "A repo with near-identical commits, repacked so that git stores most
of the commits as deltas."
  (test-git:with-git-repo (repo :dir dir)
    (test-git:make-commit repo "foo")
    (loop for i below 30
          do (test-git::run-in-dir
              repo
              (format nil "git commit -q --allow-empty -m 'A long commit message that is the same for every commit, so that git finds it worthwhile to store these commits as deltas of each other: ~a'" i)))
    (test-git::run-in-dir repo "git repack -q -adf --window=250 --depth=50")
    (test-git:enable-server-features repo)
    ;; Make sure the fixture actually tests deltas
    (assert (> (length (deltified-commits repo)) 10))
    (&body)))

(defun assert-has-all-commits (repo commits)
  (assert-that commits
               (has-length (length (all-commits repo))))
  (is (equal
       (sort (all-commits repo) #'string<)
       (sort (mapcar #'car commits) #'string<))))

(test read-commits-with-deltified-commits
  "upload-pack sends REF_DELTA entries since we don't ask for ofs-delta"
  (with-fixture deltified-repo ()
    (let ((commits (read-commits (namestring dir) :wants (list "main" "master")
                                                  :parse-parents t)))
      (assert-has-all-commits repo commits)
      (is (equal (list (git:rev-parse-local repo "HEAD^"))
                 (assoc-value commits (git:current-commit repo)
                              :test #'equal))))))

(test read-commits-with-deltified-commits-as-string
  (with-fixture deltified-repo ()
    (let ((commits (read-commits (namestring dir) :wants (list "main" "master"))))
      (assert-has-all-commits repo commits)
      (is (str:starts-with-p
           "tree "
           (assoc-value commits (git:current-commit repo)
                        :test #'equal))))))

(defun pack-objects (repo &key ofs-delta)
  "Runs git pack-objects for all commits reachable from HEAD, and returns
the pack as an octet vector."
  (uiop:with-temporary-file (:pathname pack)
    (test-git::run-in-dir
     repo
     (format nil "git rev-parse HEAD | git pack-objects -q --revs --stdout~a > ~a"
             (if ofs-delta " --delta-base-offset" "")
             (namestring pack)))
    (with-open-file (stream pack :element-type '(unsigned-byte 8))
      (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
        (read-sequence bytes stream)
        bytes))))

(defun read-pack-bytes (bytes)
  (flex:with-input-from-sequence (stream bytes)
    (read-packfile stream)))

(defun reverse-pack-entries (bytes)
  "Returns a pack with the same entries as BYTES, in reverse order. This
only makes sense for packs without OFS_DELTA entries."
  (flex:with-input-from-sequence (stream bytes)
    (let* ((stream (make-instance 'counting-input-stream :source stream))
           (num (read-packfile-header stream))
           (header-end (counting-position stream))
           (entries
             (loop for i below num
                   for start = (counting-position stream)
                   do (read-packfile-entry stream)
                   collect (subseq bytes start (counting-position stream)))))
      (apply #'concatenate '(vector (unsigned-byte 8))
             (subseq bytes 0 header-end)
             (reverse entries)))))

(defun pack-entry-types (bytes)
  "Returns an alist of entry type to the number of entries of that type
in the pack BYTES."
  (flex:with-input-from-sequence (stream bytes)
    (let* ((stream (make-instance 'counting-input-stream :source stream))
           (num (read-packfile-header stream))
           (counts nil))
      (loop for i below num
            for type = (read-packfile-entry stream)
            do (let ((cell (assoc type counts)))
                 (if cell
                     (incf (cdr cell))
                     (push (cons type 1) counts))))
      counts)))

(defun assert-commits-came-from-deltas (repo bytes delta-type)
  "Asserts that the pack BYTES has entries of DELTA-TYPE and no entries
of the other delta type, and that some commits are not stored as plain
commits, so reading all the commits must have gone through DELTA-TYPE."
  (let ((types (pack-entry-types bytes))
        (other-type (if (eql delta-type +obj-ofs-delta+)
                        +obj-ref-delta+
                        +obj-ofs-delta+)))
    (is (> (or (assoc-value types delta-type) 0) 0))
    (is (eql nil (assoc-value types other-type)))
    (is (< (or (assoc-value types +obj-commit+) 0)
           (length (all-commits repo))))))

(test read-packfile-with-ref-deltas
  (with-fixture deltified-repo ()
    (let ((bytes (pack-objects repo)))
      (assert-commits-came-from-deltas repo bytes +obj-ref-delta+)
      (assert-has-all-commits repo (read-pack-bytes bytes)))))

(test read-packfile-with-ofs-deltas
  (with-fixture deltified-repo ()
    (let ((bytes (pack-objects repo :ofs-delta t)))
      (assert-commits-came-from-deltas repo bytes +obj-ofs-delta+)
      (assert-has-all-commits repo (read-pack-bytes bytes)))))

(test read-packfile-with-deltas-before-their-bases
  (with-fixture deltified-repo ()
    (let ((bytes (reverse-pack-entries (pack-objects repo))))
      (assert-commits-came-from-deltas repo bytes +obj-ref-delta+)
      (assert-has-all-commits repo (read-pack-bytes bytes))
      ;; Chains of deltas arriving out of order are only covered if git
      ;; chose to store some, which varies between git versions and
      ;; environments (CI's Mac doesn't), so skip instead of failing.
      (unless (> (reduce #'max (deltified-commits repo) :key #'cdr) 1)
        (skip "git didn't store any commit as a chain of deltas")))))
