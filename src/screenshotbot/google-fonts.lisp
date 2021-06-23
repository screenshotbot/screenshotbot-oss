;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/google-fonts
    (:use #:cl
          #:alexandria)
  (:export #:google-fonts))

(markup:enable-reader)

;; Emacs can't parse this nicely, which is why it's in a separate file

(markup:deftag google-fonts ()
  <markup:merge-tag>
    <link rel="preconnect" href="https://fonts.gstatic.com">
<link href="https://fonts.googleapis.com/css2?family=Montserrat:ital,wght@0,400;0,500;0,600;0,700;1,400&display=swap" rel="stylesheet">
  </markup:merge-tag>)
