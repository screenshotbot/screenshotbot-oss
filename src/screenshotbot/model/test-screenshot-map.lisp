;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-screenshot-map
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot-key
                #:make-screenshot)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/model/screenshot-map
                #:screenshot-map-to-list
                #:make-screenshot-map)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:fiveam-matchers/core
                #:assert-that
                #:matchesp
                #:matcher))
(in-package :screenshotbot/model/test-screenshot-map)


(util/fiveam:def-suite)

(defun image-file (name)
  (asdf:system-relative-pathname
   :screenshotbot
   (format nil "fixture/~a" name)))

(def-fixture state ()
  (with-installation ()
   (with-test-store ()
     (let* ((channel (make-instance 'channel))
            (im-1 (make-image :pathname (image-file "wizard.png")))
            (im-2 (make-image :pathname (image-file "rose.png")))
            (screenshot-1 (make-screenshot :image im-1 :name "one"))
            (screenshot-2 (make-screenshot :image im-2 :name "two")))
       (&body)))))

(defun screenshot= (s1 s2)
  (and

   (eql (screenshot-image s1)
        (screenshot-image s2))))

(test preconditions ()
  (with-fixture state ()
    (pass)))

(defclass has-screenshot-matcher (matcher)
  ((key :initarg :key
        :reader matcher-key)))

(defun has-screenshot (screenshot)
  (make-instance 'has-screenshot-matcher
                 :key (screenshot-key screenshot)))

(defmethod matchesp ((self has-screenshot-matcher)
                     value)
  (eql :equal (fset:compare (matcher-key self)
                            (screenshot-key value))))

(test make-a-simple-list
  (with-fixture state ()
    (let ((m1 (make-screenshot-map
               channel
               (list screenshot-1
                     screenshot-2))))
      (assert-that (screenshot-map-to-list m1)
                   (contains
                    (has-screenshot screenshot-1)
                    (has-screenshot screenshot-2))))))

(test same-list-twice-in-a-row-gives-same-value
  (with-fixture state ()
    (let ((m1 (make-screenshot-map
               channel
               (list screenshot-1
                     screenshot-2)))
          (m2 (make-screenshot-map
               channel
               (list screenshot-1
                     screenshot-2))))
      (is (eql m1 m2)))))

(test second-time-is-a-different-set
  (with-fixture state ()
    (let ((m1 (make-screenshot-map
               channel
               (list screenshot-1
                     screenshot-2)))
          (m2 (make-screenshot-map
               channel
               (list screenshot-1))))
      (is (not (eql m1 m2))))))

(test image-matter-too
  (with-fixture state ()
    (let ((m1 (make-screenshot-map
               channel
               (list screenshot-1
                     screenshot-2)))
          (m2 (make-screenshot-map
               channel
               (list (make-screenshot :image im-2 :name "one")
                     (make-screenshot :image im-1 :name "two")))))
      (is (not (eql m1 m2))))))

(test recreated-screenshot
  (with-fixture state ()
    (let ((m1 (make-screenshot-map
               channel
               (list screenshot-1
                     screenshot-2)))
          (m2 (make-screenshot-map
               channel
               (list (make-screenshot :image im-1 :name "one")
                     (make-screenshot :image im-2 :name "two")))))
      (is (eql m1 m2)))))
