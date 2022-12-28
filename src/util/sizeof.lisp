;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/sizeof
  (:use #:cl)
  (:export
   #:sizeof))
(in-package :util/sizeof)

(defun sizeof (type &key imports)
  (tmpdir:with-tmpdir (tmpdir)
    (let ((input-file (make-pathname
                       :name "input"
                       :type "c"
                       :defaults tmpdir))
          (output (make-pathname
                   :name "output"
                   :defaults tmpdir)))

      (with-open-file (content input-file
                               :direction :output)
        (loop for import in (list* "stdio.h" imports)
              do (format content "#include <~a>~%" import))
        (format content "int main() {
printf(\"%d\\n\", sizeof(~a));
return 0;
}" type))

      (uiop:run-program
       (list "gcc" (namestring input-file)
             "-o" (namestring output))
       :error-output t
       :output t)
      (parse-integer
       (uiop:run-program
        (list (namestring output))
        :output 'string)))))
