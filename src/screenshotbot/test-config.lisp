;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-config
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/config
                #:load-config
                #:find-config.lisp)
  (:import-from #:screenshotbot/installation
                #:installation
                #:*installation*)
  (:import-from #:cl-mock
                #:if-called
                #:answer)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/test-config)

(util/fiveam:def-suite)

(defvar *loaded* nil)

(def-fixture state ()
  (let ((*installation* nil))
    (cl-mock:with-mocks ()
      (unwind-protect
           (&body)
        (setf *loaded* nil)))))

(test simple-load-config ()
  (with-fixture state ()
    (uiop:with-temporary-file (:pathname config :stream s :type "lisp")
      (let ((*package* (find-package :cl-user)))
        (write '(setf *loaded* t) :stream s))
      (finish-output s)
      (answer (find-config.lisp)
        config)
      (load-config)
      (is-true *loaded*))))

(test setf-installation ()
  (with-fixture state ()
    (uiop:with-temporary-file (:pathname config :stream s :type "lisp")
      (let ((*package* (find-package :cl-user)))
        (format s
                "(setf (installation) (make-instance 'installation))~%"))
      (finish-output s)
      (answer (find-config.lisp)
        config)
      (load-config)
      (is (typep *installation* 'installation)))))

(test no-config-lisp-found ()
  (with-fixture state ()
    (answer (find-config.lisp)
      nil)
    (load-config)
    (is (typep *installation* 'installation))))
