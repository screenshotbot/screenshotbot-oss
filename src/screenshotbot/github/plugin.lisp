;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/github/plugin
    (:use #:cl
          #:alexandria)
  (:import-from #:../plugin
                #:plugin
                #:plugin-parse-repo)
  (:import-from #:../model/recorder-run
                ;; todo: separate these two
                #:github-repo)
  (:export #:github-plugin
           #:github-repo
           #:app-id
           #:private-key))

(defclass github-plugin (plugin)
  ((app-id :initarg :app-id
           :accessor app-id)
   (private-key :initarg :private-key
                :accessor private-key)))

(defmethod initialize-instance :after ((plugin github-plugin)
                                       &key private-key-file &allow-other-keys)
  (when private-key-file
    (setf (private-key plugin)
          (uiop:read-file-string private-key-file))))

(let ((cache nil)
      (lock (bt:make-lock)))
  (defun make-github-repo (&key link company)
    (bt:with-lock-held (lock)
      (symbol-macrolet ((place (assoc-value cache link :test 'equal)))
        (or place
            (setf place (make-instance 'github-repo :link link
                                                    :company company)))))))

(defmethod plugin-parse-repo ((plugin github-plugin)
                              company
                              repo-str)
  (when (str:containsp "github" repo-str)
    (make-github-repo :link repo-str
                      :company company)))
