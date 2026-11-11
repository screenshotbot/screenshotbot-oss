;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/test-recovery-token
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/login/recovery-token
                #:make-recovery-token
                #:find-recovery-token
                #:recovery-token-company
                #:recovery-token-note
                #:recovery-token-expired-p
                #:recovery-tokens-for-company
                #:generate-token
                #:hash-token)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:util/store/store
                #:with-test-store))
(in-package :screenshotbot/login/test-recovery-token)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((company (make-instance 'company)))
      (&body))))

(test find-by-token
  (with-fixture state ()
    (multiple-value-bind (self token)
        (make-recovery-token company :note "T1234")
      (is (eql self (find-recovery-token token)))
      (is (eql company (recovery-token-company self)))
      (is (equal "T1234" (recovery-token-note self)))
      (is (eql nil (find-recovery-token (str:concat token "x"))))
      (is (eql nil (find-recovery-token "")))
      (is (eql nil (find-recovery-token nil))))))

(test only-the-hash-is-stored
  (with-fixture state ()
    (multiple-value-bind (self token)
        (make-recovery-token company)
      (is (not (find token
                     (mapcar (lambda (slot)
                               (let ((name (closer-mop:slot-definition-name slot)))
                                 (when (slot-boundp self name)
                                   (slot-value self name))))
                             (closer-mop:class-slots (class-of self)))
                     :test #'equal)))
      (is (equal (hash-token token)
                 (slot-value self 'screenshotbot/login/recovery-token::%token-hash))))))

(test tokens-are-unique
  (with-fixture state ()
    (is (not (equal (nth-value 1 (make-recovery-token company))
                    (nth-value 1 (make-recovery-token company)))))
    (is (eql 2 (fset:size (recovery-tokens-for-company company))))))

(test expiry
  (with-fixture state ()
    (multiple-value-bind (self token)
        (make-recovery-token company :expires-in 3600)
      (is-false (recovery-token-expired-p self))
      (is-false (recovery-token-expired-p self :now (+ (get-universal-time) 3500)))
      (is-true (recovery-token-expired-p self :now (+ (get-universal-time) 3600)))
      (setf (slot-value self 'screenshotbot/login/recovery-token::expires-at)
            (- (get-universal-time) 1))
      (is (eql nil (find-recovery-token token))))))

(test expires-in-must-be-positive
  (with-fixture state ()
    (signals error
      (make-recovery-token company :expires-in 0))
    (signals error
      (make-recovery-token company :expires-in nil))))

(test token-has-no-padding
  (loop repeat 20
        do (let ((token (generate-token)))
             (is (eql 44 (length token)))
             (is (cl-ppcre:scan "^[A-Za-z0-9_-]+$" token)))))
