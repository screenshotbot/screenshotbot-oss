;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :pixel-diff/git-diff
  (:use #:cl)
  (:import-from #:pixel-diff/image-pair
                #:updated
                #:previous
                #:image-pair)
  (:import-from #:pixel-diff/browser
                #:image-browser-window)
  (:import-from #:pixel-diff/differ
                #:load-image
                #:image-layer))
(in-package :pixel-diff/git-diff)


(defclass git-repo ()
  ((directory :initarg :directory
              :reader repo-directory)
   (rev-parse-cache :initform (make-hash-table :test #'equal)
                    :reader rev-parse-cache)))

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
  (loop for line in (str:lines
                     ($git self
                           (list "diff" "--name-status" ref1 ref2)))
        if (eql #\M (elt line 0))
          collect (str:substring 2 nil line)))

(defmethod pngs-changed ((self git-repo) ref1 ref2)
  (loop for file in (files-changed self ref1 ref2)
        if (str:ends-with-p ".png" file)
          collect file))

(defmethod rev-parse ((self git-repo) ref)
  (symbol-macrolet ((cache (gethash ref (rev-parse-cache self))))
    (or
     cache
     (setf
      cache
      (str:trim ($git self (list "rev-parse" ref)))))))

(defmethod open-git-file ((self git-repo) ref pathname &key (output :stream))
  (let ((ref (rev-parse self ref)))
    (sys:run-shell-command
     (make-git-command self "show" (format nil "~a:~a" ref pathname))
     :output output
     :if-output-exists :supersede)))

(defclass git-blob ()
  ((repo :initarg :repo
         :reader repo)
   (ref :initarg :ref
        :reader ref)
   (pathname :initarg :pathname
             :reader git-pathname)))

(defmethod load-image (pane (blob git-blob) &key editable)
  (log:debug "Loading blob: ~a:~a" (ref blob) (git-pathname blob))
  (uiop:with-temporary-file (:pathname p :type "png")
    (open-git-file (repo blob)
                   (ref blob)
                   (git-pathname blob)
                   :output p)
    (load-image pane p :editable editable)))

;; (hcl:profile (Sleep 5))




;; (pngs-changed (make-instance 'git-repo :directory "/home/arnold/builds/ios-oss/") "HEAD" "HEAD^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")

;; (open-git-file (make-instance 'git-repo :directory "/home/arnold/builds/ios-oss/") "HEAD^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^" "Kickstarter-iOS/Features/Activities/Controller/__Snapshots__/ActivitiesViewControllerTests/testMultipleSurveys_NotFacebookConnected_YouLaunched.lang_de_device_phone4_7inch.png" :output #P"/tmp/test.png")

;; (capi:contain  (make-git-diff-browser (make-instance 'git-repo :directory "/home/arnold/builds/ios-oss/") "HEAD" "HEAD^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"))


(defun make-git-diff-browser (repo ref1 ref2)
  (let ((pngs-changed (pngs-changed repo ref1 ref2)))
    (let ((image-pairs
            (loop for png in pngs-changed
                  collect
                  (make-instance 'image-pair
                                 :previous (make-instance 'git-blob
                                                          :repo repo
                                                          :ref ref1
                                                          :pathname png)
                                 :updated (make-instance 'git-blob
                                                         :repo repo
                                                         :ref ref2
                                                         :pathname png)))))
      (make-instance 'image-browser-window
                     :image1 (make-instance 'image-layer
                                            :image (previous (car image-pairs))
                                            :alpha 0)
                     :image2 (make-instance 'image-layer
                                            :image (updated (car image-pairs))
                                            :alpha 1)
                     :image-pair-list image-pairs))))



