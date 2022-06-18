;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/test-sitemap
  (:use #:cl
        #:fiveam
        #:screenshotbot/replay/sitemap)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/replay/test-sitemap)


(util/fiveam:def-suite)
(def-fixture mocked-network ()
  (cl-mock:with-mocks ()
    (cl-mock:answer dex:get
      (uiop:read-file-string (asdf:system-relative-pathname :screenshotbot "replay/fixtures/sitemap-index.xml"))
      (uiop:read-file-string (asdf:system-relative-pathname :screenshotbot "replay/fixtures/sitemap-0.xml")))
    (&body)))

(test sitemap
  (with-fixture mocked-network ()
   (is
    (str:s-member (parse-sitemap "https://www.rollins.edu/sitemap-index.xml")
                  "https://www.rollins.edu/academics/latin-american-caribbean-studies"))))
