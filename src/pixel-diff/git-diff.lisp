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
                #:image-layer)
  (:import-from #:util/threading
                #:make-thread
                #:max-pool)
  (:import-from #:alexandria
                #:when-let)
  (:export
   #:git-repo
   #:make-git-diff-browser))
(in-package :pixel-diff/git-diff)

(defvar *max-pool* (make-instance 'max-pool :max 10))

(defclass git-repo ()
  ((directory :initarg :directory
              :reader repo-directory)
   (tree-cache :initform (make-hash-table :test #'equal)
               :reader tree-cache
               :documentation "For a given rev, the tree associated with the parsed commit")))

(defmethod make-git-command ((self git-repo)
                              &rest
                                args)
  (list*
   "/usr/bin/env"
    "git" "--git-dir" (namestring
                       (path:catdir (repo-directory self) ".git/"))
    args))

(defmethod $git ((self git-repo) args &rest rest)
  (log:debug "Running: ~a" args)
  (apply #'uiop:run-program
   (apply #'make-git-command self args)
   :ignore-error-status nil
   :output 'string
   :error-output t
   rest))

(defmethod files-changed ((self git-repo) ref1 ref2)
  "Return list of modified files between REF1 and REF2.
   If REF2 is NIL, compares REF1 against the working directory.
   Returns filenames of files that have been modified"
  (loop for line in (str:lines
                     ($git self
                           (remove-if
                            #'null
                            (list "diff" "--name-status" ref1 ref2))))
        for status = (elt line 0)
        if (eql status #\M)
          collect (str:substring 2 nil line)))

(defmethod pngs-changed ((self git-repo) ref1 ref2)
  "Return list of PNG files that changed between REF1 and REF2.
   If REF2 is NIL, compares REF1 against the working directory."
  (loop for file in (files-changed self ref1 ref2)
        if (str:ends-with-p ".png" file)
          collect file))


(defmethod rev-parse ((self git-repo) ref)
  (str:trim ($git self (list "rev-parse" ref))))

(defmethod parse-tree ((self git-repo) ref)
  
  (symbol-macrolet ((cache (gethash ref (tree-cache self))))
    (or
     cache
     (setf
      cache
      (let ((commit (rev-parse self ref)))
        (log:debug "Uncached rev-parse")
        (str:trim
         ($git self (list "show" "--pretty=%T" "--no-patch" commit))))))))

(defmethod open-git-file ((self git-repo) ref pathname &key (output :stream))
  (let ((ref (parse-tree self ref)))
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


(defmethod load-image (pane (blob git-blob) callback &key editable)
  (log:debug "Loading blob: ~a:~a" (ref blob) (git-pathname blob))
  (uiop:with-temporary-file (:pathname p :type "png" :keep t)
    (make-thread
     (lambda ()
       (open-git-file (repo blob)
                      (ref blob)
                      (git-pathname blob)
                      :output p)
       (capi:apply-in-pane-process-if-alive
        pane
        (lambda ()
          (load-image pane p callback :editable editable)
          (delete-file p))))
     :pool *max-pool*)))


;; (hcl:profile (Sleep 5))




;; (pngs-changed (make-instance 'git-repo :directory "/home/arnold/builds/ios-oss/") "HEAD" "HEAD^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")

;; (open-git-file (make-instance 'git-repo :directory "/home/arnold/builds/ios-oss/") "HEAD^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^" "Kickstarter-iOS/Features/Activities/Controller/__Snapshots__/ActivitiesViewControllerTests/testMultipleSurveys_NotFacebookConnected_YouLaunched.lang_de_device_phone4_7inch.png" :output #P"/tmp/test.png")

;; 


(defun test-example ()
  (capi:contain  (make-git-diff-browser (make-instance 'git-repo :directory "/home/arnold/builds/ios-oss/") "HEAD" "HEAD^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")))

(defun make-git-diff-browser (repo ref1 ref2 &rest args)
  "Create a git diff browser comparing REF1 and REF2.
   If REF2 is NIL, compares REF1 against the working directory files.
   Returns NIL if no PNG files have changed."
  (when-let ((pngs-changed (pngs-changed repo ref1 ref2)))
    (let ((image-pairs
            (loop for png in pngs-changed
                  collect
                  (make-instance 'image-pair
                                 :previous (make-instance 'git-blob
                                                          :repo repo
                                                          :ref ref1
                                                          :pathname png)
                                 :updated (if ref2
                                              ;; Normal case: compare against another ref
                                              (make-instance 'git-blob
                                                             :repo repo
                                                             :ref ref2
                                                             :pathname png)
                                              ;; Special case: compare against working directory
                                              (path:catfile (repo-directory repo) png))))))
      (apply #'make-instance 'image-browser-window
             :image1 (make-instance 'image-layer
                                    :image (previous (car image-pairs))
                                    :alpha 0)
             :image2 (make-instance 'image-layer
                                    :image (updated (car image-pairs))
                                    :alpha 1)
             :image-pair-list image-pairs
             args))))



