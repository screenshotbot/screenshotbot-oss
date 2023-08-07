;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/diff-report
  (:use #:cl)
  (:import-from #:screenshotbot/user-api
                #:Screenshot-name)
  (:import-from #:screenshotbot/model/image
                #:image=)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-screenshots)
  (:import-from #:screenshotbot/screenshot-api
                #:screenshot-image)
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot-masks)
  (:import-from #:screenshotbot/model/image-comparer
                #:make-image-comparer)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:change
   #:before
   #:after
   #:change-masks
   #:diff-report-added
   #:diff-report-deleted
   #:diff-report-changes
   #:make-diff-report
   #:diff-report
   #:diff-report-title
   #:changes-groups
   #:added-groups
   #:deleted-groups
   #:group-items
   #:group-title
   #:actual-item
   #:group-item-subtitle))
(in-package :screenshotbot/diff-report)

(defvar *cache* (trivial-garbage:make-weak-hash-table :test #'equal
                                                      :weakness :value)
  "diff reports are generated on the fly. Generally it's fast enough
  for most use, so we don't persist it. However, the diff-reports can
  quickly become a dominant source of objects in memory (because of
  nibbles). For that reason we keep this cache, but keep the cache
  weak on the values.

  Even without weakness this would be technically still correct, since
  we won't store a diff-report more than once.")

(defclass change ()
  ((before :initarg :before
           :reader before)
   (after :initarg :after
          :reader after)
   (masks :initarg :masks
          :reader change-masks)))

(defclass diff-report ()
  ((added :initarg :added
          :reader diff-report-added
          :initform nil)
   (deleted :initarg :deleted
            :reader diff-report-deleted
            :initform nil)
   (changes :initarg :changes
            :initform nil
            :accessor diff-report-changes
            :documentation "List of all CHANGEs")
   (added-groups :initform nil
                 :accessor %added-groups)
   (deleted-groups :initform nil
                   :accessor %deleted-groups)
   (changes-groups :initform nil
                   :accessor %changes-groups)))

(defclass group-item ()
  ((subtitle :reader group-item-subtitle
             :initarg :subtitle)
   (item :initarg :actual-item
         :reader actual-item)))

(defclass group ()
  ((title :initarg :title
          :reader group-title)
   (items :initarg :items
          :reader group-items)))

(defun make-groups (items &key key subtitle)
  (let ((res (make-hash-table :test #'equal)))
    (loop for item in items
          do (push item
                   (gethash (funcall key item) res nil)))
    (loop for key being the hash-keys of res
          collect (make-instance 'group
                                  :title key
                                  :items
                                  (loop for item in (gethash key res)
                                        collect (make-instance 'group-item
                                                                :subtitle (funcall subtitle item)
                                                                :actual-item item))))))
(defun get-only-screenshot-name (screenshot)
  (car
   (str:split "--" (screenshot-name screenshot) :limit 2)))


(defun get-tab-title (screenshot)
  (cadr
   (str:split "--" (screenshot-name screenshot) :limit 2)))


(defmethod changes-groups ((self diff-report))
  (util:or-setf
   (%changes-groups self)
   (let ((changes (diff-report-changes self)))
     (make-groups changes :key (lambda (change)
                                 (get-only-screenshot-name (before change)))
                          :subtitle (lambda (change)
                                      (get-tab-title (before change)))))))

(defmethod added-groups ((self diff-report))
  (util:or-setf
   (%added-groups self)
   (let ((added (diff-report-added self)))
     (make-groups added
                  :key #'get-only-screenshot-name
                  :subtitle #'get-tab-title))))

(defmethod deleted-groups ((self diff-report))
  (util:or-setf
   (%deleted-groups self)
   (let ((deleted (diff-report-deleted self)))
     (make-groups deleted
                  :key #'get-only-screenshot-name
                  :subtitle #'get-tab-title))))


(defun diff-report-title (diff-report)
  (let ((added (added-groups diff-report))
        (deleted (deleted-groups diff-report))
        (changes (changes-groups diff-report)))
    (str:join ", "
              (remove-if 'null
               (list
                (when changes
                  (format nil "~d changes" (length changes)))
                (when added
                  (format nil "~d added" (length added)))
                (when deleted
                  (format nil "~d deleted" (length deleted))))))))

(defun hash-set-difference (left right &key test (key #'identity))
  "Similar to set-difference, but more performant"
  (let ((table (make-hash-table :test test)))
    (dolist (x left)
      (setf (gethash (funcall key x) table) x))
    (dolist (x right)
      (remhash (funcall key x) table))
    (alexandria:hash-table-values table)))

(defun %make-diff-report (run to)
  (restart-case
      (let ((names (recorder-run-screenshots run))
            (to-names (when to
                        (recorder-run-screenshots to))))
        (make-instance
         'diff-report
          :added (hash-set-difference
                  names to-names
                  :key #'screenshot-name
                  :test #'equal)
          :deleted (hash-set-difference
                    to-names names
                    :key #'screenshot-name
                    :test #'equal)
          :changes (%find-changes (make-image-comparer run) names to-names)))
    (retry-make-diff-report ()
      (make-diff-report run to))))

(defun make-diff-report (run to)
  (util:or-setf
   (gethash (list run to :v3) *cache*)
   (%make-diff-report run to)))

(defmethod %find-changes (image-comparer names to-names)
  (let ((hash-table (make-hash-table :test #'equal)))
    (loop for x in to-names
          do
          (setf (gethash (screenshot-name x) hash-table) x))
    (loop for s1 in names
          for x = (gethash (screenshot-name s1) hash-table)
          if (and
              x
              (not (image=
                    image-comparer
                    (screenshot-image s1)
                    (Screenshot-image x)
                    ;; always use the new mask
                    (screenshot-masks s1))))
            collect
            (make-instance 'change
                            :before s1
                            :masks (screenshot-masks s1)
                            :after x))))
