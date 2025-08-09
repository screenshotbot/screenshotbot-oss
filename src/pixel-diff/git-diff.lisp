;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :pixel-diff/git-diff
  (:use #:cl))
(in-package :pixel-diff/git-diff)


(defclass git-repo ()
  ((directory :initarg :directory
              :reader repo-directory)))

(defmethod make-git-command ((self git-repo)
                              &rest
                                args)
  (list*
   "/usr/bin/env"
    "git" "--git-dir" (namestring
                       (path:catdir (repo-directory self) ".git/"))
    args))

(defmethod $git ((self git-repo) args &rest rest)
  (apply #'uiop:run-program
   (apply #'make-git-command self args)
   :ignore-error-status nil
   :output 'string
   :error-output t
   rest))

(defmethod files-changed ((self git-repo) ref1 ref2)
  (str:lines
   ($git self
         (list "diff" "--name-only" ref1 ref2))))

(defmethod pngs-changed ((self git-repo) ref1 ref2)
  (loop for file in (files-changed self ref1 ref2)
        if (str:ends-with-p ".png" file)
          collect file))

(defmethod rev-parse ((self git-repo) ref)
  (str:trim ($git self (list "rev-parse" ref))))

(defmethod open-git-file ((self git-repo) ref pathname &key (output :stream))
  (let ((ref (rev-parse self ref)))
    (sys:run-shell-command
     (make-git-command self "show" (format nil "~a:~a" ref pathname))
     :output output
     :if-output-exists :supersede)))





;; (pngs-changed (make-instance 'git-repo :directory "/home/arnold/builds/ios-oss/") "HEAD" "HEAD^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")

;; (open-git-file (make-instance 'git-repo :directory "/home/arnold/builds/ios-oss/") "HEAD^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^" "Kickstarter-iOS/Features/Activities/Controller/__Snapshots__/ActivitiesViewControllerTests/testMultipleSurveys_NotFacebookConnected_YouLaunched.lang_de_device_phone4_7inch.png" :output #"/tmp/test.png")
