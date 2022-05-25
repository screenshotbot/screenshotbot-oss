;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :cl-user)

(ql:quickload :screenshotbot.sdk)

(defun output-file ()
  (car (asdf:output-files 'asdf:compile-op (asdf:find-component :screenshotbot.sdk.deliver "deliver-sdk"))))

(format t "We'll be writing to ~A~%" (output-file))

(ensure-directories-exist (output-file))

(defun safe-delete-file (x)
  (when (uiop:file-exists-p x)
    (delete-file x)))

(defun deliver-main ()
  (let ((output-file (output-file)))
    #-darwin ;; universal binary, output file should be temporary
    (safe-delete-file output-file)
    (deliver 'screenshotbot/sdk/main:main
              output-file
              5
              :keep-function-name t
              :keep-debug-mode t
              :keep-pretty-printer t
              :keep-clos-object-printing t
              :keep-lisp-reader t
              ;; temporary: get the build green
              :keep-eval t
              :keep-symbols `(system:pipe-exit-status)
              :packages-to-keep-symbol-names :all
              :multiprocessing t)))

(compile 'deliver-main)

#-darwin
(deliver-main)

#+darwin
(cond
  ((hcl:building-universal-intermediate-p)
   (deliver-main))
  (t
   (safe-delete-file (output-file))
   (hcl:save-universal-from-script "src/screenshotbot/sdk/deliver-sdk.lisp")))

(uiop:quit)
