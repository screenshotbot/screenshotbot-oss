;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/factory
    (:use #:cl
          #:alexandria
          #:screenshotbot/template
          #:screenshotbot/user-api
          #:screenshotbot/screenshot-api
          #:screenshotbot/report-api
          #:screenshotbot/git-repo
          #:screenshotbot/model/github
          #:screenshotbot/api-key-api)
  (:import-from #:screenshotbot/github/access-checks
                #:github-repo)
  (:import-from #:util/object-id
                #:make-oid)
  (:import-from #:screenshotbot/model/recorder-run
                #:compare-tolerance
                #:compare-threshold)
  (:import-from #:screenshotbot/model/api-key
                #:expires-at
                #:api-key-description)
  (:export #:test-user
           #:test-company
           #:test-screenshot
           #:test-image
           #:test-channel))
(in-package :screenshotbot/factory)



(defclass test-channel ()
  ((name :initform "dummy-channel"
         :initarg :name
         :accessor channel-name)
   (%created-at :initform 10
                :accessor %created-at)
   (object-id :initform 1
              :accessor bknr.datastore:store-object-id)
   (repo :initarg :repo
         :initform (make-instance 'github-repo :link "https://github.com/tdrhq/foo.git")
         :accessor channel-repo)))

(defclass test-image ()
  ((url :initarg :url
        :initform "https://foo"
        :accessor %image-public-url)))

(defmethod image-public-url ((image test-image) &key &allow-other-keys)
  (%image-public-url image))


(defclass test-screenshot ()
  ((name :initarg :name
         :accessor screenshot-name)))

