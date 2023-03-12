;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-screenshot-key
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/screenshot
                #:make-screenshot
                #:make-key-from-screenshot
                #:make-screenshot-from-key)
  (:import-from #:screenshotbot/model/screenshot-key
                #:ensure-screenshot-key
                #:screenshot-key)
  (:import-from #:screenshotbot/user-api
                #:screenshot-name)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:import-from #:screenshotbot/screenshot-api
                #:screenshot-image))
(in-package :screenshotbot/model/test-screenshot-key)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test simple-conversion
  (with-fixture state ()
    (let ((screenshot (make-screenshot :name "foobar2")))
      (is (eql
           (make-key-from-screenshot screenshot)
           (make-key-from-screenshot screenshot)))
      (is (equal "foobar2"
                 (screenshot-name
                  (make-key-from-screenshot screenshot))))
      (assert-that (make-key-from-screenshot screenshot)
                   (has-typep 'screenshot-key)))))

(test reverse-conversion
  (with-fixture state ()
    (let ((key (ensure-screenshot-key :name "bleh")))
      (is (eql key (ensure-screenshot-key :name "bleh")))
      (is (not (eql key (ensure-screenshot-key :name "bleh"
                                               :lang "blah"))))
      (let ((screenshot (make-screenshot-from-key key :image)))
        (is (equal "bleh" (screenshot-name screenshot)))
        (is (eql :image (screenshot-image screenshot)))))))

(defun to-alist (map)
  (let ((ret))
   (fset:do-map (key value map (reverse ret))
     (push (cons key value) ret))))

(test can-make-map
  (with-fixture state ()
    (let ((s1 (ensure-screenshot-key :name "bleh"))
          (s2 (ensure-screenshot-key :name "car")))
      (is (eql :less
               (fset:compare s1 s2)))
      (let ((map
              (fset:with
               (fset:with
                (fset:empty-map)
                s1 "foo")
               s2 "bar")))

        (is (equal "foo" (fset:lookup map s1)))
        (is (equal "bar" (fset:lookup map s2)))
        (is (equal (list
                    (cons s1 "foo")
                    (cons s2 "bar"))
                   (to-alist map)))))))


(test can-make-with-equivalent-keys
  (with-fixture state ()
    (let ((s1 (ensure-screenshot-key :name "bleh"))
          (s2 (ensure-screenshot-key :name "bleh")))
      (is (eql :equal
               (fset:compare s1 s2)))
      (let ((map
              (fset:with
               (fset:with
                (fset:empty-map)
                s1 "foo")
               s2 "bar")))

        (is (equal "bar" (fset:lookup map s1)))
        (is (equal "bar" (fset:lookup map s2)))
        (is (equal (list
                    (cons s2 "bar"))
                   (to-alist map)))))))


(test large-set
  (with-fixture state ()
    (let* ((rstate (make-random-state) #|for determinism|#)
           (map (fset:empty-map))
           (keys (loop for i from 0 to 1000
                       collect
                       (ensure-screenshot-key :name
                                              (format nil "~a"
                                                      (random 1000000000
                                                              rstate))))))

      (loop for key in keys
            do (setf map (fset:with map key "foo")))
      (is
       (equal
        (mapcar #'car (to-alist map))
        (sort keys #'string< :key #'screenshot-name))))))
