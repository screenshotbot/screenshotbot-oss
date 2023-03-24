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
                #:lite-screenshot)
  (:import-from #:screenshotbot/model/screenshot-key
                #:screenshot-key)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/model/screenshot-map
                #:chain-cost
                #:compute-cost
                #:*lookback-count*
                #:pick-best-existing-map
                #:*delta-factor*
                #:screenshots
                #:make-from-previous
                #:deleted
                #:to-map
                #:screenshot-map-to-list
                #:make-screenshot-map)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:assert-that
                #:matchesp
                #:matcher)
  (:import-from #:screenshotbot/model/screenshot-key
                #:ensure-screenshot-key
                #:screenshot-key)
  (:import-from #:util/object-id
                #:oid)
  (:import-from #:screenshotbot/screenshot-api
                #:screenshot-image
                #:make-screenshot)
  (:import-from #:fiveam-matchers/described-as
                #:described-as)
  (:import-from #:fiveam-matchers/satisfying
                #:satisfying)
  (:import-from #:fiveam-matchers/misc
                #:is-null))
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
            (screenshot-2 (make-screenshot :image im-2 :name "two"))
            (screenshot-key-1-copy
              (make-instance 'screenshot-key :name "one"))
            (screenshot-key-2-copy
              (make-instance 'screenshot-key :name "two"))
            (screenshot-key-3
              (make-instance 'screenshot-key :name "three")))
       (&body)))))

(defun screenshot= (s1 s2)
  (and
   (eql :equal
        (fset:compare (screenshot-key s1)
                      (screenshot-key s2)))
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

(test to-map-is-cached
  (with-fixture state ()
    (let ((m1 (make-screenshot-map
               channel
               (list screenshot-1
                     screenshot-2))))
      (is
       (eql (to-map m1)
            (to-map m1))))))


(defun set-to-list (set &optional res)
  (cond
    ((fset:empty? set)
     res)
    (t
     (let ((least (fset:least set)))
       (set-to-list
        (fset:less set least)
        (list*
         least
         res))))))

(test recreated-screenshot-with-uneql-objects
  (with-fixture state ()
    (let ((m1 (make-screenshot-map
               channel
               (list screenshot-1
                     screenshot-2)))
          (m2 (make-screenshot-map
               channel
               (list (make-instance 'lite-screenshot
                                    :screenshot-key screenshot-key-1-copy
                                    :image-oid (oid im-1 :stringp nil))
                     (make-instance 'lite-screenshot
                                    :screenshot-key screenshot-key-2-copy
                                    :image-oid (oid im-2 :stringp nil))
                     (make-instance 'lite-screenshot
                                    :screenshot-key screenshot-key-3
                                    :image-oid (oid im-2 :stringp nil))))))
      (is (not (eql m2 m1)))
      (let ((map-2 (to-map m2))
            (map-1 (to-map m1)))
        (is (fset:equal?
             map-1
             (fset:map-intersection map-2 map-1)))
        (let ((diff (fset:map-difference-2 map-2 map-1)))
          (is (eql 1 (fset:size diff))))

        ;; All of the above tests should go through even if we don't use
        ;; the same map core. So now, we ensure that we're actually
        ;; using the same core. The two maps have different
        ;; screenshot-keys, so we want to make sure the new map has the
        ;; same screenshot-key's as the first.
        (let ((keys-1 (set-to-list (fset:domain map-1)))
              (keys-2 (set-to-list (fset:domain map-2))))

          ;; the sorting order is two, three, one (reverse sorted)
          (is (eql screenshot-key-3 (elt keys-2 1)))

          (assert-that keys-1
                       (contains
                        (screenshot-key screenshot-2)
                        (screenshot-key screenshot-1)))
          (assert-that keys-2
                       (contains
                        (screenshot-key screenshot-2)
                        screenshot-key-3
                        (screenshot-key screenshot-1))))))))


(test recreated-screenshot-with-deleted-uneql-objects
  (with-fixture state ()
    (let ((*delta-factor* 100))
     (let* ((screenshot-3 (make-screenshot
                           :key (ensure-screenshot-key :name "foobar")
                           :image im-1))
            (m1 (make-screenshot-map
                 channel
                 (list screenshot-1
                       screenshot-2
                       screenshot-3)))
            (m2 (make-screenshot-map
                 channel
                 (list (make-instance 'lite-screenshot
                                      :screenshot-key screenshot-key-1-copy
                                      :image-oid (oid im-1 :stringp nil))))))
       (is (not (eql m2 m1)))
       (let ((map-2 (to-map m2))
             (map-1 (to-map m1)))
         (is (fset:equal?
              map-2
              (fset:map-intersection map-2 map-1)))
         (let ((diff (fset:map-difference-2 map-2 map-1)))
           (is (eql 0 (fset:size diff))))
         (let ((diff (fset:map-difference-2 map-1 map-2)))
           (is (eql 2 (fset:size diff))))

         ;; All of the above tests should go through even if we don't use
         ;; the same map core. So now, we ensure that we're actually
         ;; using the same core. The two maps have different
         ;; screenshot-keys, so we want to make sure the new map has the
         ;; same screenshot-key's as the first.
         (let ((keys-1 (set-to-list (fset:domain map-1)))
               (keys-2 (set-to-list (fset:domain map-2))))

           (assert-that (deleted m2)
                        (described-as
                            "We expect the deleted slot to be set"
                          (contains
                           (screenshot-key screenshot-2)
                           (screenshot-key screenshot-3))))

           (assert-that keys-1
                        (contains
                         (screenshot-key screenshot-2)
                         (screenshot-key screenshot-1)
                         (screenshot-key screenshot-3)))
           (assert-that keys-2
                        (contains
                         (screenshot-key screenshot-1)))))))))

(test make-from-previous
  (with-fixture state ()
    (let* ((m1 (make-from-previous (list screenshot-1)
                                   nil channel))
           (m2 (make-from-previous (list screenshot-1 screenshot-2)
                                   m1 channel)))
      (assert-that (screenshots m1)
                   (contains
                    (satisfying (screenshot= screenshot-1 *))))
      (assert-that (screenshots m2)
                   (contains
                    (satisfying (screenshot= screenshot-2 *)))))))

(test make-from-previous-with-deleted-items
  (with-fixture state ()
    (let* ((m1 (make-from-previous (list screenshot-1)
                                   nil channel))
           (m2 (make-from-previous (list screenshot-2)
                                   m1 channel)))
      (assert-that (screenshots m1)
                   (contains
                    (satisfying (screenshot= screenshot-1 *))))
      (assert-that (screenshots m2)
                   (contains
                    (satisfying (screenshot= screenshot-2 *))))
      (assert-that (deleted m2)
                   (contains
                    (is-equal-to
                     (screenshot-key screenshot-1)))))))

(test pick-best-existing-map
  (with-fixture state ()
    (let* ((m1 (make-from-previous (list screenshot-1) nil channel))
           (m2 (make-from-previous (list screenshot-2) nil channel)))
      (assert-that (pick-best-existing-map channel
                                           (list screenshot-1))
                   (is-equal-to
                    m1))
      (assert-that (pick-best-existing-map channel
                                           (list screenshot-2))
                   (is-equal-to
                    m2))
      (assert-that
       (let ((*lookback-count* 1))
         (pick-best-existing-map channel
                                 (list screenshot-1)))
       (is-equal-to
        m2)))))

(test compute-cost
  (with-fixture state ()
    (let* ((one (fset:with
                 (fset:empty-map)
                 (ensure-screenshot-key :name "foo")
                 :one))
           (two (fset:with
                 (fset:empty-map)
                 (ensure-screenshot-key :name "bar")
                 :two))
           (three (fset:with
                   (fset:empty-map)
                   (ensure-screenshot-key :name "foo")
                   :two))
           (four (fset:with
                  three
                  (ensure-screenshot-key :name "bar")
                  :two)))
      (is (eql 2 (compute-cost one two)))
      (is (eql 1 (compute-cost one three)))
      (is (eql 2 (compute-cost two one)))
      (is (eql 1 (compute-cost three one)))
      (is (eql 2 (compute-cost one four)))
      (is (eql 2 (compute-cost four one)))
      (is (eql 1 (compute-cost two four)))
      (is (eql 1 (compute-cost four two))))))

(def-fixture chain-costs ()
  (let* ((one (make-from-previous (list screenshot-1 screenshot-2)
                                    nil channel))
           (two (make-from-previous (list screenshot-1)
                                    one channel))
           (three (make-from-previous (list screenshot-2)
                                      two channel)))
      (&body)))

(test chain-cost
  (with-fixture state ()
    (with-fixture chain-costs ()
      (is (eql 0 (chain-cost nil)))
      (is (eql 2 (chain-cost one)))
      (is (eql 3 (chain-cost two)))
      (is (eql 5 (chain-cost three))))))

(test chain-cost-uncached
  (with-fixture state ()
    (with-fixture chain-costs ()
      (is (eql 5 (chain-cost three)))
      (is (eql 2 (slot-value one 'chain-cost)))
      (is (eql 3 (slot-value two 'chain-cost)))
      (is (eql 5 (slot-value three 'chain-cost))))))
