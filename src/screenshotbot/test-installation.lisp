;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/test-installation
    (:use #:cl
          #:alexandria
          #:fiveam
          #:screenshotbot/installation)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:core/config/api
                #:config))

(util/fiveam:def-suite)

(defclass my-plugin () ())

(test find-plugin
  (let ((plugin (make-instance 'my-plugin)))
    (is (eql plugin (find-plugin (make-instance 'installation
                                                 :plugins (list plugin))
                                 'my-plugin)))
    (signals simple-error
      (find-plugin (make-instance 'installation)
                   'my-plugin))))

(test default-oidc-provider-happy-path-with-config
  (with-test-store ()
   (let ((installation (make-instance 'installation)))
     (is (eql nil (default-oidc-provider installation)))
     (setf (config "sso.oidc.client-id") "abcd")
     (is (eql nil (default-oidc-provider installation)))
     (setf (config "sso.oidc.client-secret") "car")
     (setf (config "sso.oidc.issuer")
           "https://example.com/")
     (is (not (null (default-oidc-provider installation)))))))
