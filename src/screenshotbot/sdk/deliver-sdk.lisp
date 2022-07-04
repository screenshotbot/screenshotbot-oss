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

#+lispworks
(defun include-template-file ()
  (let ((path (asdf:system-relative-pathname :screenshotbot.sdk "template.lisp")))
    (with-open-file (out path :direction :output
                              :if-exists :supersede)

      (fli:start-collecting-template-info)
      (handler-case
          (screenshotbot/sdk/sdk::update-commit-graph (screenshotbot/sdk/git::git-repo) "bar")
        (screenshotbot/sdk/sdk::api-error ()
          nil))
      (fli:print-collected-template-info :output-stream out))
    (let ((output (compile-file path)))
      (load output))))

(defun deliver-main ()
  (let ((output-file (output-file)))
    #-darwin ;; universal binary, output file should be temporary
    (safe-delete-file output-file)
    #+lispworks
    (include-template-file)

    #+ccl
    (error "unimplemented")

    #+sbcl
    (sb-ext:save-lisp-and-die
     output-file
     :toplevel 'screenshotbot/sdk/main:main
     :executable t
     :save-runtime-options t)

    #+lispworks
    (deliver 'screenshotbot/sdk/main:main
              output-file
              5
              :keep-function-name t
              #+mswindows :console #+mswindows :init
              #+mswindows :startup-bitmap-file #+mswindows nil
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
