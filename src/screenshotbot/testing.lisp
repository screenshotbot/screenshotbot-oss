;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/testing
    (:use #:cl
          #:alexandria)
  (:import-from #:./model/company
                #:company)
  (:import-from #:./model/user
                #:user)
  (:import-from #:./model/api-key
                #:api-key)
  (:import-from #:bknr.datastore
                #:delete-object))

(defmacro with-test-user ((&key (company (gensym "company"))
                                (company-class '(quote company))
                             (user (gensym "user"))
                             (api-key (gensym "api-key"))) &body body)
  `(let* ((,company (make-instance ,company-class))
          (,user (make-instance 'user :companies (list company)))
          (,api-key (make-instance 'api-key :user user
                                            :company company))
          (*events* nil))
     (declare (ignorable ,company ,user ,api-key))
     (unwind-protect
          (progn ,@body)
       (delete-object ,api-key))))
