;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :util)

(defun load-countries ()
  (let* ((csv (path:catfile (util:system-source-directory :util) "./countries.csv"))
         (values (cl-csv:read-csv csv)))
    (loop for val in (cdr values)
       collect (cadr val))))

(defun load-states ()
  (let* ((csv (path:catfile (util:system-source-directory :util) "./states.csv"))
         (values (cl-csv:read-csv csv)))
    (loop for value in (cdr values)
         collect
         (format nil "~A ~A" (cadr value) (car value)))))
