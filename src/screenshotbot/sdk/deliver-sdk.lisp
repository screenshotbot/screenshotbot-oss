;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :cl-user)

(ql:quickload :screenshotbot.sdk)
(ql:quickload :core.cli/deliver)

(defun output-file ()
  (car (asdf:output-files 'asdf:compile-op (asdf:find-component :screenshotbot.sdk/deliver "deliver-sdk"))))

(defun template-builder ()
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
        (list "attrib" "-r" (format nil "~a\*.*" (namestring dir)) "/s")))))

(compile 'template-builder)

(core/cli/deliver:deliver-end-user-cli
 :restart-fn 'screenshotbot/sdk/main:main
 :deliver-script *load-truename*
 :output-file (output-file)
 :template-builder-fn #'template-builder)
