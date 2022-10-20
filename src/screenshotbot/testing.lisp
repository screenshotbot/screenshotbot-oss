;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/testing
    (:use #:cl
          #:alexandria)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/user
                #:user)
  (:import-from #:screenshotbot/model/api-key
                #:api-key)
  (:import-from #:bknr.datastore
                #:delete-object)
  (:import-from #:screenshotbot/model/user
                #:make-user)
  (:import-from #:screenshotbot/user-api
                #:current-company
                #:current-user)
  (:import-from #:screenshotbot/login/common
                #:*current-company-override*)
  (:import-from #:util/testing
                #:with-fake-request))

(defmacro with-test-user ((&key (company (gensym "company"))
                                (company-name "Dummy org")
                                (company-class '(quote company))
                             (user (gensym "user"))
                             (api-key (gensym "api-key"))
                             (logged-in-p nil)) &body body)
  `(let* ((,company (make-instance ,company-class
                                   :name ,company-name))
          (,user (make-user :companies (list ,company)))
          (,api-key (make-instance 'api-key :user ,user
                                            :company ,company))
          (*events* nil))
     (declare (ignorable ,company ,user ,api-key))
     (flet ((body ()
              (unwind-protect
                   (progn ,@body)
                (delete-object ,api-key))))
       (cond
         (,logged-in-p
          (with-fake-request ()
            (auth:with-sessions ()
              (setf (current-user) ,user)
              (let ((*current-company-override* ,company))
                (body)))))
         (t
          (body))))))
