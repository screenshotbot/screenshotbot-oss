;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/cleanup
  (:use #:cl)
  (:import-from #:screenshotbot/model/image
                #:mask-rect
                #:image-blob
                #:image)
  (:import-from #:screenshotbot/dashboard/compare
                #:image-comparison)
  (:import-from #:util/store
                #:find-any-refs)
  (:import-from #:screenshotbot/diff-report
                #:hash-set-difference)
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/cleanup)

(defun delete-unreferenced-images ()
  (let ((types '(screenshot
                 image
                 image-blob
                 mask-rect)))
    (let ((objects
            (loop for type in types
                  appending (bknr.datastore:store-objects-with-class type))))
      (let* ((refs (find-any-refs objects))
             (deletable (hash-set-difference objects refs
                                             :test #'eql)))
        (restart-case
            (when deletable
              (error "~d out of ~d elements can be deleted: ~S"
                     (length deletable)
                     (length objects)
                     deletable))
          (delete-all-these-objects ()
            (bknr.datastore:snapshot) ;; just to be sure
            (loop for x in deletable
                  do (bknr.datastore:delete-object x))))))))

;; (delete-unreferenced-images)
