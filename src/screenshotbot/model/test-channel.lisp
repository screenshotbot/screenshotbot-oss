;; -*- coding: utf-8 -*-
;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/model/test-channel
  (:use #:cl
        #:alexandria
        #:screenshotbot/model/channel
        #:screenshotbot/model/recorder-run
        #:screenshotbot/model/company
        #:fiveam)
  (:import-from #:screenshotbot/model/channel
                #:shortened-channel-name
                #:channel-active-commits
                #:repos-for-company
                #:channels-for-company
                #:review-policy-name
                #:review-policy
                #:%review-policy
                #:get-full-repo-from-repo)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:fiveam-matchers/strings
                #:contains-string)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:import-from #:screenshotbot/model/recorder-run
                #:runs-for-channel
                #:unchanged-run
                #:make-recorder-run)
  (:import-from #:screenshotbot/model/review-policy
                #:disallow-author-review-policy
                #:anyone-can-review)
  (:import-from #:fiveam-matchers/lists
                #:contains
                #:contains-in-any-order)
  (:import-from #:auth/viewer-context
                #:api-viewer-context
                #:normal-viewer-context)
  (:import-from #:screenshotbot/model/api-key
                #:api-key)
  (:import-from #:screenshotbot/user-api
                #:user))

(util/fiveam:def-suite)

(defclass unserializable () ())

(def-fixture state ()
  (with-test-store ()
    (let* ((channel (make-instance 'channel))
           (run (make-recorder-run
                 :channel channel)))
      (&body))))

(test active-runs
  (with-fixture state ()
   (is (equal nil (active-run channel "foo")))
    (setf (active-run channel "foo") run)
    (is (eql run (active-run channel "foo")))))

(test production-run-for
  (with-fixture state ()
   (let* ((company (make-instance 'company))
          (channel (make-instance 'channel
                                   :company company)))
     (is (fset:empty? (runs-for-channel channel)))
     (let ((run (make-recorder-run :channel channel
                                   :company company)))
       (is (equal (list run)
                  (fset:convert 'list
                   (runs-for-channel channel))))))))

(test find-production-run-for-commit
  (with-fixture state ()
   (let* ((channel (make-instance 'channel))
          (foo-run (make-recorder-run :channel channel
                                      :commit-hash "foo"
                                      :trunkp t))
          (bar-run (make-recorder-run :channel channel
                                      :commit-hash "bar"
                                      :trunkp t)))
     (is (eql foo-run (production-run-for channel :commit "foo")))
     (is (eql bar-run (production-run-for channel :commit "bar")))
     (let ((another-bar-run (make-recorder-run
                             :channel channel
                             :commit-hash "bar")))
       (is (eql bar-run (production-run-for channel :commit "bar")))))))

(test find-production-run-for-commit-does-not-pick-non-production-runs
  (with-fixture state ()
   (let* ((channel (make-instance 'channel))
          (foo-run (make-recorder-run :channel channel
                                      :commit-hash "foo"
                                      :trunkp nil))
          (bar-run (make-recorder-run :channel channel
                                      :commit-hash "bar"
                                      :trunkp nil)))
     (is (eql nil (production-run-for channel :commit "foo")))
     (is (eql nil (production-run-for channel :commit "bar")))
     (let ((another-bar-run (make-recorder-run
                             :channel channel
                             :commit-hash "bar"
                             :trunkp t)))
       (is (eql another-bar-run (production-run-for channel :commit "bar")))))))

(test find-production-run-unchanged-run
  (with-fixture state ()
   (let* ((channel (make-instance 'channel))
          (unchanged-run (make-instance 'unchanged-run
                                  :channel channel
                                  :commit "foo"
                                  :other-commit "bar"))
          (bar-run (make-recorder-run :channel channel
                                      :commit-hash "bar"
                                      :trunkp t)))
     (is (eql bar-run (production-run-for channel :commit "bar")))
     (is (eql bar-run (production-run-for channel :commit "foo")))

     (let ((another-bar-run (make-recorder-run
                             :channel channel
                             :commit-hash "bar")))
       (is (eql bar-run (production-run-for channel :commit "foo")))
       (is (eql bar-run (production-run-for channel :commit "bar")))))))

(test finds-production-run-if-theres-also-an-unchanged-run-for-that-commit
  (with-fixture state ()
   (let* ((channel (make-instance 'channel))
          (unchanged-run (make-instance 'unchanged-run
                                  :channel channel
                                  :commit "bar"
                                  :other-commit "bar"))
          (bar-run (make-recorder-run :channel channel
                                      :commit-hash "bar"
                                      :trunkp t)))
     (is (eql bar-run (production-run-for channel :commit "bar")))
     (is (eql nil (production-run-for channel :commit "foo"))))))

(test detect-infinite-loops-in-unchanged-run
  (with-fixture state ()
   (let* ((channel (make-instance 'channel))
          (foo-run (make-instance 'unchanged-run
                                  :channel channel
                                  :commit "foo"
                                  :other-commit "bar"))
          (bar-run (make-instance 'unchanged-run
                                  :channel channel
                                  :commit "bar"
                                  :other-commit "foo")))
     (is (eql nil (production-run-for channel :commit "bar"))))))

(test get-full-repo-from-repo
  (is (equal "foo/bar"
             (get-full-repo-from-repo "git@github.com:foo/bar.git"))))


(test channel-print-object
  (with-fixture state ()
    (is (equal "#<CHANNEL foobar>"
               (prin1-to-string
                (make-instance 'channel
                               :name "foobar"))))
    (assert-that (prin1-to-string (make-instance 'channel))
                 (contains-string "CHANNEL"))))

(test review-policy-happy-path
  (with-fixture state ()
    (let ((channel (make-instance 'channel :name "foobar")))
      (assert-that (review-policy channel)
                   (has-typep 'anyone-can-review))
      (slot-makunbound channel '%review-policy)
      (assert-that (review-policy channel)
                   (has-typep 'anyone-can-review))
      (setf (review-policy-name channel) :disallow-author)
      (assert-that (review-policy channel)
                   (has-typep 'disallow-author-review-policy)))))

(test sets-was-promoted-p
  (with-fixture state ()
    (is-false (was-promoted-p run))
    (setf (active-run channel "master") run)
    (is-true (was-promoted-p run))
    (setf (active-run channel "master") (make-recorder-run :channel channel
                                                           :screenshots nil))
    (is-true (was-promoted-p run))))

(test channels-for-company
  (with-fixture state ()
    (let ((channel-1
            (make-instance 'channel
                           :company :company-1))
          (channel-2
            (make-instance 'channel
                           :company :company-1))
          (channel-3
            (make-instance 'channel
                           :company :company-3)))
      (assert-that
       (channels-for-company :company-1)
       (contains-in-any-order channel-1 channel-2))
      (assert-that
       (channels-for-company :company-3)
       (contains channel-3)))))

(test repos-for-company
  (with-fixture state ()
   (let ((channel-1
           (make-instance 'channel
                          :company :company-1
                          :github-repo "https://github.com/tdrhq/fast-example.git"))
         (channel-2
           (make-instance 'channel
                          :company :company-1
                          :github-repo "https://gitlab.com/foo/bar.git"))
         (channel-3
           (make-instance 'channel
                          :company :company-3
                          :github-repo "https://github.com/tdrhq/bleh.git")))
     (assert-that
      (repos-for-company :company-1)
      (contains-in-any-order "https://github.com/tdrhq/fast-example.git"
                             "https://gitlab.com/foo/bar.git")))))

(test repos-for-company-removes-duplicates
  (with-fixture state ()
   (let ((channel-1
           (make-instance 'channel
                          :company :company-1
                          :github-repo "https://github.com/tdrhq/fast-example.git"))
         (channel-2
           (make-instance 'channel
                          :company :company-1
                          :github-repo "https://github.com/tdrhq/fast-example.git"))
         (channel-3
           (make-instance 'channel
                          :company :company-3
                          :github-repo "https://github.com/tdrhq/bleh.git")))
     (assert-that
      (repos-for-company :company-1)
      (contains-in-any-order "https://github.com/tdrhq/fast-example.git")))))

(test active-commits
  (with-fixture state ()
    (is (equal nil (channel-active-commits channel)))
    (let ((run (make-recorder-run
                :channel channel
                :work-branch "foobar"
                :commit-hash "abcd")))
      (setf (active-run channel "foobar") run)
      (is (equal (list "abcd") (channel-active-commits channel))))
    (let ((run (make-recorder-run
                :channel channel
                :work-branch "car"
                :commit-hash "aaaa")))
      (setf (active-run channel "car") run)
      (assert-that (channel-active-commits channel)
                   (contains-in-any-order "aaaa" "abcd" )))
    (let ((run (make-recorder-run
                :channel channel
                :work-branch "car"
                :commit-hash nil)))
      (setf (active-run channel "dar") run)
      (assert-that (channel-active-commits channel)
                   (contains-in-any-order "aaaa" "abcd" )))
    (let ((run (make-recorder-run
                :channel channel
                :work-branch "car"
                :commit-hash "")))
      (setf (active-run channel "dar") run)
      (assert-that (channel-active-commits channel)
                   (contains-in-any-order "aaaa" "abcd" )))))

(test active-commits-removes-duplicates
  (with-fixture state ()
    (is (equal nil (channel-active-commits channel)))
    (let ((run (make-recorder-run
                :channel channel
                :work-branch "foobar"
                :commit-hash "abcd")))
      (setf (active-run channel "foobar") run)
      (is (equal (list "abcd") (channel-active-commits channel))))
    (let ((run (make-recorder-run
                :channel channel
                :work-branch "car"
                :commit-hash "abcd")))
      (setf (active-run channel "car") run)
      (assert-that (channel-active-commits channel)
                   (contains-in-any-order "abcd" )))))

(test shortened-channel-name
  (is (equal "foobar" (shortened-channel-name "foobar")))
  (is (equal "foo/bar" (shortened-channel-name "foo/bar")))
  (is (equal "foo/.../la/car"
             (shortened-channel-name
              "foo/bar/dar/blah/la/car"
              :length 14)))
  (is (equal "foo/.../la/c:ar"
             (shortened-channel-name
              "foo/bar/dar/blah/la/c:ar"
              :length 15)))
  (is (equal "foo/...ah/la/car"
             (shortened-channel-name
              "foo/bar/dar/blah/la/car"
              :length 16)))
  (is (equal "...ar"
             (shortened-channel-name "foobar" :length 5)))
  (is (equal "...la/car"
             (shortened-channel-name
              "foobar/bar/dar/blah/la/car"
              :length 9))))

(test shortened-channel-name-for-gradle
  (is (equal "foobar" (shortened-channel-name "foobar")))
  (is (equal "foo:bar" (shortened-channel-name "foo:bar")))
  (is (equal "foo:...:la:car"
             (shortened-channel-name
              "foo:bar:dar:blah:la:car"
              :length 14)))
  (is (equal "foo:...:la:c/ar"
             (shortened-channel-name
              "foo:bar:dar:blah:la:c/ar"
              :length 15)))
  (is (equal "foo:...ah:la:car"
             (shortened-channel-name
              "foo:bar:dar:blah:la:car"
              :length 16)))
  (is (equal "...ar"
             (shortened-channel-name "foobar" :length 5)))
  (is (equal "...la:car"
             (shortened-channel-name
              "foobar:bar:dar:blah:la:car"
              :length 9))))

;; ----------------------------------------------------------------------
;; Who may change a channel's settings
;; ----------------------------------------------------------------------

(def-fixture editing ()
  (with-test-store ()
    (let* ((company (make-instance 'company))
           (other-company (make-instance 'company))
           (user (make-instance 'user))
           (channel (make-instance 'channel :company company)))
      (roles:ensure-has-role company user 'roles:standard-member)
      (flet ((browser (user)
               (make-instance 'normal-viewer-context :user user))
             (api (user company)
               (make-instance 'api-viewer-context
                              :user user
                              :api-key (make-instance 'api-key
                                                      :user user
                                                      :permissions '(:full)
                                                      :company company))))
        (&body)))))

(test a-standard-member-may-edit-their-companys-channel
  (with-fixture editing ()
    (is-true (auth:can-viewer-edit (browser user) channel))))

(test a-read-only-member-may-view-but-may-not-edit
  "The distinction this method exists to draw. Before it, CAN-VIEWER-EDIT
had no channel method at all and answered NIL for everyone."
  (with-fixture editing ()
    (roles:ensure-has-role company user 'roles:guest)
    (is-true (auth:can-viewer-view (browser user) channel))
    (is-false (auth:can-viewer-edit (browser user) channel))))

(test someone-outside-the-company-may-not-edit
  (with-fixture editing ()
    (let ((outsider (make-instance 'user)))
      (is-false (auth:can-viewer-edit (browser outsider) channel)))))

(test an-api-key-may-edit-only-its-own-companys-channels
  "A user can belong to several companies; the key is issued for one."
  (with-fixture editing ()
    (roles:ensure-has-role other-company user 'roles:standard-member)
    (is-true (auth:can-viewer-edit (api user company) channel))
    (is-false (auth:can-viewer-edit (api user other-company) channel))))

(test a-public-channel-is-readable-by-outsiders-but-not-editable
  "PUBLICP says anyone may look, not that anyone may reconfigure."
  (with-fixture editing ()
    (setf (publicp channel) t)
    (let ((outsider (make-instance 'user)))
      (is-true (auth:can-viewer-view (api outsider other-company) channel))
      (is-false (auth:can-viewer-edit (api outsider other-company) channel)))))
