;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/testing
    (:use #:cl
          #:alexandria)
  (:import-from #:screenshotbot/model/company
                #:get-singleton-company
                #:prepare-singleton-company
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
  (:import-from #:util/testing
                #:define-screenshot-test-init
                #:with-global-binding
                #:screenshot-static-page
                #:with-fake-request)
  (:import-from #:util/object-id
                #:oid-array)
  (:import-from #:screenshotbot/model/image
                #:with-local-image
                #:image)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/installation
                #:multi-org-feature
                #:installation
                #:*installation*)
  (:import-from #:screenshotbot/server
                #:screenshotbot-template)
  (:export
   #:snap-image-blob
   #:snap-all-images
   #:fix-timestamps
   #:screenshot-test
   #:multi-org-test-installation))

(defclass multi-org-test-installation (multi-org-feature
                                       installation)
  ())

(def-easy-macro with-installation (&key globally
                                        (installation (make-instance 'installation))
                                        &fn fn)
  (cond
    ((boundp '*installation*)
     (funcall fn))
    (globally
     (with-global-binding ((*installation* installation))
       (funcall fn)))
    (t
     (let ((*installation* installation))
       (funcall fn)))))

(defmacro with-test-user ((&key (company (gensym "company"))
                                (company-name "dummy company")
                                (company-class '(quote company))
                             (user (gensym "user"))
                             (api-key (gensym "api-key"))
                             (logged-in-p nil)
                             (fake-request-args nil)) &body body)
  `(with-installation ()
     (let* ((,company
              (typecase (installation)
                (multi-org-feature
                 (make-instance ,company-class
                                :name ,company-name))
                (t
                 (prepare-singleton-company)
                 (get-singleton-company (installation)))))
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
            (with-fake-request (,@fake-request-args)
              (auth:with-sessions ()
                (setf (auth:request-user hunchentoot:*request*) ,user)
                (setf (auth:request-account hunchentoot:*request*)
                      ,company)
                (setf (auth:viewer-context hunchentoot:*request*)
                      (make-instance 'auth/viewer-context:normal-viewer-context
                                     :user ,user))
                (body))))
           (t
            (body)))))))


(defun snap-image-blob (im1)
  "Copy the givem image blob into the screenshot assets"
  (let ((dir (asdf:system-relative-pathname
              :screenshotbot
              "static-web-output/image/blob/")))
    (let ((output (path:catfile
                   dir
                   (format nil "~a/default.webp"
                           (encrypt:encrypt-mongoid (oid-array im1))))))
      (with-local-image (file im1)
        (uiop:copy-file file (ensure-directories-exist output))))))

(defun snap-all-images ()
  (loop for image in (bknr.datastore:class-instances 'image)
        do (snap-image-blob image)))

(defun fix-timestamps (node)
  (mquery:with-document (node)
    (mquery:remove-class (mquery:$ "time") "timeago")
    (dolist (time (mquery:$ "time"))
     (setf (mquery:text time) "Some timestamp"))))

(defmacro screenshot-test (name &body body)
  `(fiveam:test ,name
     (screenshot-static-page
      :screenshotbot
      (str:downcase ,(string name))
      (let ((core/ui/template:*app-template*
              (make-instance 'screenshotbot-template)))
        (progn
          ,@body)))))

(define-screenshot-test-init :screenshotbot
  :static-assets "static/assets/"
  :generated-css-assets
  `((:screenshotbot.css-assets . "assets/css/default.css")
    #-screenshotbot-oss
    (:screenshotbot.pro.css/extended-dashboard . "assets/css/extended-dashboard.css")
    #-screenshotbot-oss
    (:screenshotbot.pro.css . "assets/css/new-landing.css")))
