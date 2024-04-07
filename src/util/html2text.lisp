;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage #:util/html2text
  (:use #:cl)
  (:export #:html2text))
(in-package #:util/html2text)

(markup:enable-reader)

(defun html2text (markup)
  (let* ((markup (if (stringp markup) markup
                     (markup:write-html markup))))
    (html2text:html2text markup)))
