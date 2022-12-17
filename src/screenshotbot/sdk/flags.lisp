;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/sdk/flags
  (:use #:cl
        #:alexandria)
  (:import-from #:com.google.flag
                #:parse-string)
  (:import-from #:screenshotbot/sdk/common-flags
                #:define-flag)
  (:use-reexport
   #:screenshotbot/sdk/common-flags)
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
   #:*static-website*
   #:*browser-configs*
   #:*static-website-assets-root*
   #:*commit-hash*
   #:*override-commit-hash*
   #:*selenium-hub*
   #:*selenium-hub-port*
   #:*firebase-output*
   #:*self-test*))

(in-package :screenshotbot/sdk/flags)

(define-flag *directory*
  :default-value "./"
  :selector "directory"
  :type string
  :help "Directory of images")

(define-flag *org-defaults*
  :default-value nil
  :selector "defaults"
  :type (or null string))


(define-flag *create-github-issue*
  :selector "create-github-issue"
  :default-value nil
  :type boolean
  :help "Create a Github issue if enabled on your account")


(define-flag *gitlab-merge-request-iid*
  :selector "gitlab-merge-request-iid"
  :default-value nil
  :type (or null string)
  :help "GitLab merge request IID")


(define-flag *channel*
  :selector "channel"
  :default-value "unnamed-channel"
  :type string
  :help "Channel name for screenshot tracking. Defaults to `unnamed-channel`.")

(define-flag *pull-request*
  :selector "pull-request"
  :default-value nil
  :type (or null string)
  :help "Pull request URL. Automatically detected on CircleCI,
  Bitrise, Netlify.")

(define-flag *branch*
  :selector "branch"
  :default-value nil
  :type (or null string)
  :help "[OBSOLETE]")

(define-flag *main-branch*
  :selector "main-branch"
  :default-value nil
  :type (or null string)
  :help "Git Branch of the main branch being tracked. We try first
  `main` and then `master`, by checking for origin/<branch-name>")

(define-flag *repo-url*
  :selector "repo-url"
  :default-value nil
  :type (or null string)
  :help "Repository URL (e.g. https://github.com/foo/bar)")

(define-flag *phabricator-diff-id*
  :selector "phabricator-diff-id"
  :default-value nil
  :type (or null string)
  :help "Phabricator Diff ID")

(define-flag *build-url*
  :selector "build-url"
  :default-value nil
  :type (or null string)
  :help "Build URL to easily identify build that generated this run")

(define-flag *production*
  :selector "production"
  :default-value t
  :type boolean
  :documentation "Whether this is a run on your CI. For local runs, we
  suggest using `--production=false`. This avoids polluting your runs
  in production.")

(define-flag *lang-regex*
  :selector "lang-regex"
  :default-value nil
  :type (or null string))

(define-flag *device-regex*
  :selector "device-regex"
  :default-value nil
  :type (or null string)
  :documentation "[OBSOLETE]")

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
  :type (or null string list)
  :parser parse-string
  :help "A metadata.xml file (Android only)")

(define-flag *static-website*
  :selector "static-website"
  :default-value nil
  :type (or null string)
  :help "Use to generate screenshots of a static website")

(define-flag *static-website-assets-root*
  :selector "static-website-assets-root"
  :default-value nil
  :type (or null string)
  :help "When parsing the website directory at --static-website, the
  asset root is used to determine where to fetch JS, CSS and image
  from. If not specified, we assume the assets are all stored in the
  same directory.")

(define-flag *browser-configs*
  :selector "browser-configs"
  :default-value nil
  :type (or null string)
  :help "A YAML file that specifies the configuration of the
  browsers. Please see documentation for details.")

(define-flag *override-commit-hash*
  :selector "override-commit-hash"
  :default-value nil
  :type (or null string)
  :help "Override the commit hash detected by git. In most cases you
  don't need this, and this is only relevant for Pull Requests, and
  only if you rebase your changes as part of your CI run. This hash
  must be the full hash, partial hashes or tag names are not
  suitable.
  Automatically detected on: CircleCI, Bitrise")

(define-flag *firebase-output*
  :selector "firebase-output"
  :default-value nil
  :type (or null string)
  :help "When running Android tests in Firebase Test Lab, pass the output of
 the `gcloud firebase test android run` as a file to this option. We'll
 automatically fetch the required files from Google Cloud, and clean
 up  the files from Google Cloud when we're done. We use the `gcloud`
 command line tool so you must have already called
 activate-service-account before this step.")

(define-flag *self-test*
  :selector "self-test"
  :default-value nil
  :type boolean
  :help "Run self-diagnostic tools to ensure that this CLI can work on your
machine.")
