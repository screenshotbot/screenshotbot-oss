;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/env
  (:use #:cl)
  (:import-from #:alexandria
                #:assoc-value
                #:when-let
                #:if-let)
  (:shadow #:getenv)
  (:export
   #:make-env-reader
   #:api-key
   #:api-secret
   #:api-hostname
   #:pull-request-url
   #:sha1
   #:build-url
   #:repo-url
   #:guess-channel-name))
(in-package :screenshotbot/sdk/env)

(defclass base-env-reader ()
  ((overrides :initarg :overrides
              :reader overrides))
  (:documentation "Reads the environment to get some information about the current build"))

(defclass env-reader (base-env-reader)
  ())

(defmethod getenv ((self base-env-reader) name)
  (if (slot-boundp self 'overrides)
   (assoc-value (overrides self)
                name :test #'string=)
   (uiop:getenv name)))

(defmethod api-key ((self base-env-reader))
  (getenv self "SCREENSHOTBOT_API_KEY"))

(defmethod api-secret ((self base-env-reader))
  (getenv self "SCREENSHOTBOT_API_SECRET"))

(defmethod api-hostname ((self base-env-reader))
  (getenv self "SCREENSHOTBOT_API_HOSTNAME"))

(defmethod pull-request-url ((self env-reader))
  nil)

(defmethod guess-channel-name ((self base-env-reader))
  nil)

(defmethod build-url ((self env-reader))
  ;; For jenkins
  (getenv self "BUILD_URL"))

(defmethod repo-url ((self env-reader))
  nil)

(defmethod sha1 ((self env-reader))
  nil)

(defgeneric validp (env-reader)
  (:documentation "Is the current environment reader valid for this situation"))

(defclass circleci-env-reader (base-env-reader)
  ())

(defmethod build-url ((self circleci-env-reader))
  (getenv self "CIRCLE_BUILD_URL"))

(defmethod repo-url ((self circleci-env-reader))
  (getenv self "CIRCLE_REPOSITORY_URL"))

(defmethod validp ((self circleci-env-reader))
  (or
   (sha1 self)
   (pull-request-url self)))

(defmethod pull-request-url ((self circleci-env-reader))
  (getenv self "CIRCLE_PULL_REQUEST"))

(defmethod sha1 ((self circleci-env-reader))
  (getenv self "CIRCLE_SHA1"))

(defclass bitrise-env-reader (base-env-reader)
  ())

(defmethod validp ((self bitrise-env-reader))
  (or
   (pull-request-url self)
   (sha1 self)))

(defmethod build-url ((self bitrise-env-reader))
  (getenv self "BITRISE_BUILD_URL"))

(defmethod pull-request-url ((self bitrise-env-reader))
  (if-let ((repo-url (repo-url self))
           (pull-id (getenv self "BITRISE_PULL_REQUEST")))
    (link-to-github-pull-request repo-url pull-id)))

(defmethod repo-url ((Self bitrise-env-reader))
  (or
   ;; TODO: this one is probably incorrect.
   (getenv self "BITRISEIO_PULL_REQUEST_REPOSITORY_URL")
   (getenv self "GIT_REPOSITORY_URL")))

(defun link-to-github-pull-request (repo-url pull-id)
  (let ((key (cond
               ((str:containsp "bitbucket" repo-url)
                "pull-requests")
               (t
                "pulls"))))
   (format nil "~a/~a/~a"
           repo-url
           key
           pull-id)))

(defclass netlify-env-reader (base-env-reader)
  ())

(defmethod pull-request-url ((self netlify-env-reader))
  (let ((pull-request-p (equal "true" (getenv self "PULL_REQUEST"))))
    (when pull-request-p
      (when-let ((review-id (getenv self "REVIEW_ID")))
        (link-to-github-pull-request
         (repo-url self)
         review-id)))))

(defmethod build-url ((self netlify-env-reader))
  (let ((build-id (getenv self "BUILD_ID"))
        (site-name (getenv self "SITE_NAME")))
    (format nil "https://app.netlify.com/sites/~a/deploys/~a"
            site-name
            build-id)))

(defmethod repo-url ((self netlify-env-reader))
  (getenv self "REPOSITORY_URL"))

(defmethod guess-channel-name ((self netlify-env-reader))
  (getenv self "SITE_NAME"))

(defmethod sha1 ((self netlify-env-reader))
  (getenv self "COMMIT_REF"))

(defmethod validp ((self netlify-env-reader))
  (equal "true" (getenv self "NETLIFY")))

(defmethod validp ((self env-reader))
  t)

(defmethod sha1 ((self bitrise-env-reader))
  (getenv self "BITRISE_GIT_COMMIT"))

(defun make-env-reader ()
  (loop for option in '(circleci-env-reader
                        bitrise-env-reader
                        netlify-env-reader)
        for env = (make-instance option)
        if (validp env)
          return env
        finally
           (return (make-instance 'env-reader))))
