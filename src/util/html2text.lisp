;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :util)

(markup:enable-reader)

(defun html2text (markup)
  (let* ((markup (if (stringp markup) markup
                     (markup:write-html markup)))
         (input-stream (make-string-input-stream markup))
         (output-stream (make-string-output-stream)))
    (uiop:run-program (list "html2text")
                      :input input-stream
                      :output output-stream)
    (get-output-stream-string output-stream)))
