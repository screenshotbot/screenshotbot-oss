;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/sdk/flags
  (:use #:cl
        #:com.google.flag
        #:alexandria)
  (:export
   #:*directory*
   #:*verbose*
   #:*org-defaults*
   #:*api-key*
   #:*create-github-issue*
   #:*api-secret*
   #:*gitlab-merge-request-iid*
   #:*hostname*
   #:*channel*
   #:*pull-request*
   #:*branch*
   #:*main-branch*
   #:*repo-url*
   #:*phabricator-diff-id*
   #:*build-url*
   #:*production*
   #:*help*
   #:*lang-regex*
   #:*device-regex*
   #:*ios-multi-dir*
   #:*ios-diff-dir*
   #:*metadata*
   #:*static-website*))

(in-package :screenshotbot/sdk/flags)

(define-flag *directory*
  :default-value "./"
  :selector "directory"
  :type string
  :help "Directory of images")

(define-flag *verbose*
   :default-value nil
   :selector "verbose"
   :type boolean
   :help "Verbose logs")

(define-flag *org-defaults*
  :default-value nil
  :selector "defaults"
  :type (or null string))

(define-flag *api-key*
  :selector "api-key"
  :default-value nil
  :type (or null string)
  :help "Screenshotbot API Key")

(define-flag *create-github-issue*
  :selector "create-github-issue"
  :default-value nil
  :type boolean
  :help "Create a Github issue if enabled on your account")

(define-flag *api-secret*
  :selector "api-secret"
  :default-value nil
  :type (or null string)
  :help "Screenshotbot API Secret")

(define-flag *gitlab-merge-request-iid*
  :selector "gitlab-merge-request-iid"
  :default-value nil
  :type (or null string)
  :help "GitLab merge request IID")

(define-flag *hostname*
  :selector "api-hostname"
  :default-value "https://api.screenshotbot.io"
  :type string
  :help "Screenshotbot API Endpoing"
  :documentation "Only used for Enterprise User")

(define-flag *channel*
  :selector "channel"
  :default-value "unnamed-channel"
  :type string
  :help "Channel name for screenshot tracking")

(define-flag *pull-request*
  :selector "pull-request"
  :default-value nil
  :type (or null string)
  :help "Pull request URL")

(define-flag *branch*
  :selector "branch"
  :default-value nil
  :type (or null string)
  :help "Git Branch name for screenshot tracking")

(define-flag *main-branch*
  :selector "main-branch"
  :default-value nil
  :type (or null string)
  :help "Git Branch of the main branch being tracked")

(define-flag *repo-url*
  :selector "repo-url"
  :default-value nil
  :type (or null string)
  :help "Repository URL (e.g. https://github.com/foo/bar)")

(define-flag *phabricator-diff-id*
  :selector "phabricator-diff-id"
  :default-value nil
  :type (or null string)
  :help "PHabricator Diff ID")

(define-flag *build-url*
  :selector "build-url"
  :default-value nil
  :type (or null string)
  :help "Build URL to easily identify build that generated this run")

(define-flag *production*
  :selector "production"
  :default-value t
  :type boolean)

(define-flag *help*
  :selector "help"
  :default-value nil
  :type boolean)

(define-flag *lang-regex*
  :selector "lang-regex"
  :default-value nil
  :type (or null string))

(define-flag *device-regex*
  :selector "device-regex"
  :default-value nil
  :type (or null string))

(define-flag *ios-multi-dir*
  :selector "ios-multi-dir"
  :default-value nil
  :type boolean)

(define-flag *ios-diff-dir*
  :selector "ios-diff-dir"
  :default-value nil
  :type (or null string)
  :help
  "When using ios-snapshot-test-case, this can link to the
  IMAGE_DIFF_DIR. In most cases this isn't required, but it can be
  useful for backward compatibility if you don't want to update the
  tests to always work in recorde mode.")

(define-flag *metadata*
  :selector "metadata"
  :default-value nil
  :type (or null string)
  :help "A metadata.xml file (Android only)")

(define-flag *static-website*
  :selector "static-website"
  :default-value nil
  :type (or null string)
  :help "Use to generate screenshots of a static website")
