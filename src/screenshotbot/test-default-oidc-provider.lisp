;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-default-oidc-provider
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/default-oidc-provider
                #:default-oidc-provider
                #:active-default-oidc-provider
                #:activep
                #:activate-default-oidc-provider
                #:deactivate-default-oidc-provider)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/store/object-id
                #:oid
                #:find-by-oid)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/test-default-oidc-provider)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (flet ((make-provider ()
             (make-instance 'default-oidc-provider
                            :issuer "https://example.com/"
                            :client-id "client-id"
                            :client-secret "client-secret"
                            :scope "openid email profile")))
      (&body))))

(test new-providers-are-not-active
  (with-fixture state ()
    (let ((provider (make-provider)))
      (is-false (activep provider))
      (is (eql nil (active-default-oidc-provider))))))

(test can-find-by-oid
  (with-fixture state ()
    (let ((provider (make-provider)))
      (is (eql provider (find-by-oid (oid provider)))))))

(test activate-and-deactivate
  (with-fixture state ()
    (let ((provider (make-provider)))
      (activate-default-oidc-provider provider)
      (is-true (activep provider))
      (is (eql provider (active-default-oidc-provider)))
      (deactivate-default-oidc-provider provider)
      (is-false (activep provider))
      (is (eql nil (active-default-oidc-provider))))))

(test activating-replaces-the-active-provider
  (with-fixture state ()
    (let ((one (make-provider))
          (two (make-provider)))
      (activate-default-oidc-provider one)
      (activate-default-oidc-provider two)
      (is-false (activep one))
      (is-true (activep two))
      (is (eql two (active-default-oidc-provider))))))

(test activating-the-active-provider-again
  (with-fixture state ()
    (let ((provider (make-provider)))
      (activate-default-oidc-provider provider)
      (activate-default-oidc-provider provider)
      (is (eql provider (active-default-oidc-provider))))))
