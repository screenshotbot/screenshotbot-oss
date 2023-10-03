;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :cl-user)

(ql:quickload :screenshotbot.sdk)

(defun output-file ()
  (car (asdf:output-files 'asdf:compile-op (asdf:find-component :screenshotbot.sdk/deliver "deliver-sdk"))))

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
      (tmpdir:with-tmpdir (dir)
        (unwind-protect
             (let ((git-repo (make-instance 'screenshotbot/sdk/git::git-repo
                                             :dir dir
                                             :link "https://github.com/tdrhq/fast-example")))
               (screenshotbot/sdk/git::$
                 (list "git" "clone" "https://github.com/tdrhq/fast-example"
                       dir))
               (handler-case
                   (screenshotbot/sdk/sdk::update-commit-graph
                    (make-instance 'screenshotbot/sdk/api-context:api-context
                                   :key "deliver-sdk"
                                   :secret "xxx"
                                   :hostname "https://screenshotbot.io")
                    git-repo "bar")
                 (screenshotbot/sdk/sdk::api-error ()
                   nil)))
          #+mswindows
          (screenshotbot/sdk/git::$
            (list "attrib" "-r" (format nil "~a\*.*" (namestring dir)) "/s"))))
      (fli:print-collected-template-info :output-stream out))
    (let ((output (compile-file path)))
      (load output))))

(defun sentry-client::lw-find-dspec-location (frame)
  nil)

(compile 'sentry-client::lw-find-dspec-location)

(defun deliver-main ()
  (when (find-package :cl+ssl)
    (error "CL+SSL is present in this image, this can lead to problems when delivering this
on Mac. (e.g., the image will try to load libcrypto etc."))
  (when (find-package :bknr.cluster/server)
    (error "BKNR.CLUSTER is present in this image"))
  (let ((output-file (output-file)))
    #-darwin ;; universal binary, output file should be temporary
    (safe-delete-file output-file)
    #+lispworks
    (include-template-file)

    (setf asdf:*central-registry* nil)
    (setf ql:*local-project-directories* nil)

    ;; Causes a warning early on from cl+ssl not finding libcrypto or
    ;; some such.
    (lw:undefine-action "When starting image" "Reset cl-mongo-id state")

    ;; See Lisp Support Call #43182
    ;; The goal is to allow us to run the CLI on Alpine Linux (or any linux that
    ;; is shipped with musl instead of glibc.)
    #+(and linux arm64)
    (setf (symbol-function 'sys::set-signals-mask) #'lw:false)

    (build-utils/deliver-script:default-deliver 'screenshotbot/sdk/main:main
              output-file
              5
              :keep-function-name t
              :keep-debug-mode nil
              #+mswindows :console #+mswindows :init
              #+mswindows :startup-bitmap-file #+mswindows nil
              :keep-clos :meta-object-slots
              :keep-pretty-printer t
              :keep-clos-object-printing t
              :kill-dspec-table nil ;; for defadvice, in particular clingon:exit
              :keep-lisp-reader t
              ;; temporary: get the build green
              :keep-symbols `(system:pipe-exit-status
                              dspec:find-dspec-locations)
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
