;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :util)

(markup:enable-reader)

(defun html2text (markup)
  (uiop:with-temporary-file (:stream input-stream
                             :pathname input-pathname
                             :prefix "input-html"
                             :direction :io :external-format :utf-8)
   (let* ((markup (if (stringp markup) markup
                      (markup:write-html markup))))
     (write-string markup input-stream)
     (finish-output input-stream)

     (uiop:with-temporary-file (:pathname output-pathname
                                :prefix "output-html")
      (uiop:run-program (list "html2text" "-utf8")
                        :input input-pathname
                        :output output-pathname)
      (uiop:read-file-string output-pathname :external-format :utf-8)))))
