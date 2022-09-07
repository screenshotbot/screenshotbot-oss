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
  (:import-from #:screenshotbot/model/image-comparison
                #:image-comparison-after
                #:image-comparison-before
                #:image-comparison)
  (:import-from #:util/store
                #:find-any-refs)
  (:import-from #:screenshotbot/diff-report
                #:hash-set-difference)
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot)
  (:import-from #:bknr.indices
                #:object-destroyed-p)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/cleanup)

(defun bad? (x)
  (or (null x)
      (object-destroyed-p x)))

#|

It's a little manual, but the current process of clearning up images
involves multiple passes. (In the future, once we're confident that
they work correctly, we'll automate it all.)

First, we DELETE-UNREFERNCED-IMAGES. Verify that the output looks
reasonable, and call the DELETE-ALL-THESE-OBJECTS restart. Next we
call DELETE-ORPHANED-IMAGE-COMPARISONS. This function is relatively
safe since it deletes objects that are essentially caches. Finally
call DELETE-UNREFERENCED-IMAGES again.

But we're not done, we still need to delete unused images. Consider
delaying this step if the disk usage isn't critical, since image blobs
don't have transaction logs.

|#

(defun delete-orphaned-image-comparisons ()
  (loop for ic in (bknr.datastore:store-objects-with-class 'image-comparison)
        if (or
            (bad? (image-comparison-before ic))
            (bad? (image-comparison-after ic)))
          do
             (bknr.datastore:delete-object ic)))

;; (delete-orphaned-image-comparisons)

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
;; (bknr.datastore::delete-orphaned-blob-files nil)
