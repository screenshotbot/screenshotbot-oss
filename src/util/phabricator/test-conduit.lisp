;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/phabricator/test-conduit
  (:use #:cl
        #:fiveam)
  (:import-from #:util/phabricator/conduit
                #:serialize-params)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/phabricator/test-conduit)

(util/fiveam:def-suite)

(test serialize-params
  (is (equal '(("foo" . "bar"))
             (serialize-params '(("foo" . "bar")))))
  (is (equal '(("foo" . "bar")
               ("car[0]" . "far")
               ("car[1]" . "bleh"))
             (serialize-params '(("foo" . "bar")
                                 ("car" . ("far" "bleh"))))))
    (is (equal '(("foo" . "bar")
               ("car[0][deet]" . "far")
               ("car[1][deet]" . "bleh"))
             (serialize-params `(("foo" . "bar")
                                 ("car" . (,(a:plist-hash-table '("deet" "far"))
                                            ,(a:plist-hash-table '("deet" "bleh")))))))))

(test serialize-number
  (is (equal '(("revision_id" . "5996" ))
             (serialize-params '(("revision_id" . 5996))))))
