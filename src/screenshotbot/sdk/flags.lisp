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
   #:*create-github-issue*
   #:*gitlab-merge-request-iid*
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
   #:*self-test*
   #:*commit-limit*
   #:*mark-failed*
   #:*versionp*
   #:*compare-threshold*
   #:*desktop*
   #:*recursive*
   #:*unchanged-from*
   #:*finalize*
   #:*batch*
   #:*work-branch*
   #:*main-branch-commit-hash*
   #:*tags*))

(in-package :screenshotbot/sdk/flags)

(define-flag *directory*
  :default-value nil
  :selector "directory"
  :type (or null string)
  :help "Directory of images")

(define-flag *recursive*
  :default-value nil
  :selector "recursive"
  :type boolean
  :help "Whether to scan the directory recursively")

(define-flag *versionp*
  :default-value nil
  :selector "version"
  :type boolean
  :help "Show the version of this SDK and exit")

(define-flag *org-defaults*
  :default-value nil
  :selector "defaults"
  :type (or null string)
  :help "[OBSOLETE]")

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

(define-flag *main-branch-commit-hash*
  :selector "main-branch-commit-hash"
  :default-value nil
  :type (or null string)
  :help "The commit of the main branch at time of running. In most case, you
DO NOT need to provide this, and we'll detect this from the Git repository
directly. Please contact support@screenshotbot.io if you plan on using this.")

(define-flag *work-branch*
  :selector "work-branch"
  :default-value nil
  :type (or null string)
  :help "Your current branch, preferably as provided by your CI environment
as opposed to reading from git. Some CI environments may not update the
local branch name, and instead provides the branch name as an environment
variable. We can automatically figure this out on CircleCI, Bitrise,
Netlify, Azure DevOps, BuildKits, and Bitbucket Pipelines.")

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
  :help "Whether this is a run on your CI. For local runs, we
  suggest using `--production=false`. This avoids polluting your runs
  in production.")

(define-flag *lang-regex*
  :selector "lang-regex"
  :default-value nil
  :type (or null string)
  :help "[OBSOLETE]")

(define-flag *device-regex*
  :selector "device-regex"
  :default-value nil
  :type (or null string)
  :help "[OBSOLETE]")

(define-flag *ios-multi-dir*
  :selector "ios-multi-dir"
  :default-value nil
  :help "[OBSOLETE]"
  :type boolean)

(define-flag *ios-diff-dir*
  :selector "ios-diff-dir"
  :default-value nil
  :type (or null string)
  :help "[OBSOLETE]")

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

(define-flag *batch*
  :selector "batch"
  :default-value nil
  :type (or null string)
  :help "Batch multiple channels into one single build status named by this
argument.")

(define-flag *self-test*
  :selector "self-test"
  :default-value nil
  :type boolean
  :help "Run self-diagnostic tools to ensure that this CLI can work on your
machine.")

(define-flag *commit-limit*
  :selector "commit-limit"
  :default-value 1000
  :type (or null integer)
  :help "Limit the number of commits for which we upload commit hashes. This is
useful for large repositories (about > 10000 commits). For large
repositories, sending all the commit hashes will time out. The
uploaded graph is merged with the graph that the server already knows
of.

You should set this high enough so that Screenshotbot always has the
entire commit graph from the previous promoted run to the new run. A
value of about 1000 should be safe and relatively fast for most
people.")

(define-flag *mark-failed*
  :selector "mark-failed"
  :default-value nil
  :type boolean
  :help "Mark this run as failed. This might happen if the build step that
generates the screenshots failed.

You don't need to call this, but if you do we can use the information
to show more appropriate information on Pull Requests. For instance,
if you have a Pull Request based off of a failing commit, we can find
the last green commit to make our screenshot report.")

(define-flag *unchanged-from*
  :selector "mark-unchanged-from"
  :default-value nil
  :type (or null string)
  :help "Notify Screenshotbot that the run for this commit will be identical
to the run from the commit provided")

(define-flag *compare-threshold*
  :selector "compare-threshold"
  :default-value nil
  :type (or null double-float)
  :help "Fraction of pixels that can be different for Screenshotbot to consider
the screenshots to be the same. (between 0.0 and 1.0)

We don't recommend using this unless absolutely required. Flaky
screenshots are harder to maintain long term. Using this argument can
also slow down the processig of your reports significantly.

If not specified, or if specified by 0.0, we'll consider screenshots
equal only if they are identical by file content. (e.g. any changes in
encoding, or EXIF data will cause a screenshot change.)

Keep in mind that this value will typically be very low. e.g., a
1000x1000 image, using 0.001 threshold would still allow for 1000
pixel changes which might be too high for most practical uses. You
probably want to choose this so that no more than 10-20 pixels are
allowed to be different at a time.")

(define-flag *desktop*
  :selector "desktop"
  :default-value nil
  :type boolean
  :help "Whether running in desktop mode. In desktop mode the --hostname
defaults to http://localhost:4095, and authentication is disabled.")

(define-flag *finalize*
  :selector "finalize"
  :default-value nil
  :type boolean
  :help "Notify Screenshotbot that all builds on all channels on this
commit are complete. This is not required to be called, but if
used provides a better developer experience when later builds
 are waiting on this commit.")

(define-flag *tags*
  :selector "tags"
  :default-value nil
  :type (or null string)
  :help "A comma separated list of tags to associate with these runs. Tags are arbitrary and will be shown next to the run names, and let's you filter runs by tags.")
