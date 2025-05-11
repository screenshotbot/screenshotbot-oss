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
                #:read-netrc
                #:supported-remote-repo-p
                #:read-commits
                #:parse-parents
                #:make-upload-pack-command)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:local-nicknames (#:test-git #:screenshotbot/sdk/test-git)))
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

(test azure-is-not-supported
  (is-false (supported-remote-repo-p "git@ssh.dev.azure.com:v3/testsbot/fast-example/fast-example"))
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
