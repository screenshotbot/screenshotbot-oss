;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mcp/test-runs
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/mcp/runs
                #:+max-log-characters+
                #:+max-screenshots+)
  (:import-from #:screenshotbot/mcp/test-util
                #:add-channel
                #:call-tool-as
                #:caller
                #:company
                #:decode
                #:field
                #:static-asset
                #:token
                #:token-with
                #:tool-text)
  (:import-from #:screenshotbot/model/channel
                #:channel)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:screenshotbot/model/recorder-run
                #:active-run
                #:make-recorder-run
                #:promotion-log)
  (:import-from #:screenshotbot/model/screenshot
                #:make-screenshot)
  (:import-from #:util/store/object-id
                #:oid)
  (:documentation "The fetch_run and fetch_active_run tools."))
(in-package :screenshotbot/mcp/test-runs)

(util/fiveam:def-suite)

(defun make-run (company channel &key (screenshots 1))
  (make-recorder-run
   :company company
   :channel channel
   :commit-hash "abc123"
   ;; Deliberately different, because the tool reports them as separate
   ;; fields and a test that set them alike could not tell them apart.
   :branch "main"
   :work-branch "feature-x"
   :author "someone"
   :tags (list "nightly")
   :build-url "https://ci.example.com/1"
   :screenshots
   (loop for i below screenshots
         collect (make-screenshot
                  :name (format nil "screen-~a" i)
                  :image (make-image
                          :company company
                          :pathname (static-asset
                                     "assets/images/example-view.svg.png"))))))

(defun fetch-run-as (token id)
  (tool-text (call-tool-as token "fetch_run" (list (cons "run_id" id)))))

(defun fetch-active-run-as (token name)
  (tool-text (call-tool-as token "fetch_active_run" (list (cons "channel" name)))))

;; ----------------------------------------------------------------------
;; fetch_run
;; ----------------------------------------------------------------------

(test a-run-lists-its-screenshots-with-image-ids
  "The point of the tool: enough for a model to identify the run and then
ask for the images it wants to look at."
  (with-fixture caller ()
    (let* ((channel (add-channel "web"))
           (run (make-run company channel :screenshots 2)))
      (multiple-value-bind (text result) (fetch-run-as token (oid run))
        (is-false (field result "isError"))
        (let* ((json (decode text))
               (screenshots (field json "screenshots")))
          (is (equal (oid run) (field json "id")))
          (is (equal "web" (field json "channel")))
          (is (equal "abc123" (field json "commit")))
          (is-true (str:containsp "/runs/" (field json "url")))
          (is (equal 2 (length screenshots)))
          (is (equal "screen-0" (field (first screenshots) "name")))
          ;; The ids a model then feeds to fetch_image_url.
          (is-true (field (first screenshots) "imageId")))))))

(test the-main-branch-and-the-work-branch-are-reported-separately
  "The run model's BRANCH slot is the *main* branch despite the name, and
conflating the two would tell a model a pull request was built on main."
  (with-fixture caller ()
    (let* ((channel (add-channel "web"))
           (run (make-run company channel)))
      (let ((json (decode (fetch-run-as token (oid run)))))
        (is (equal "main" (field json "mainBranch")))
        (is (equal "feature-x" (field json "workBranch")))))))

(test the-image-ids-a-run-hands-out-are-the-ids-fetch-image-url-accepts
  "fetch_run is only useful composed with fetch_image_url, and nothing else
checks that one's output is the other's input."
  (with-fixture caller ()
    (let* ((channel (add-channel "web"))
           (run (make-run company channel)))
      (let* ((json (decode (fetch-run-as token (oid run))))
             (image-id (field (first (field json "screenshots")) "imageId")))
        (is-true image-id)
        (multiple-value-bind (text result)
            (tool-text (call-tool-as token "fetch_image_url"
                                     (list (cons "image_id" image-id))))
          (is-false (field result "isError")
                    "image id ~a did not resolve" image-id)
          (is-true (str:containsp "http" text)))))))

(test fetching-an-unknown-run-is-a-tool-error-not-a-crash
  "A model hands us whatever id it has."
  (with-fixture caller ()
    (dolist (id (list "not-an-oid" "" "000000000000000000000000"))
      (multiple-value-bind (text result) (fetch-run-as token id)
        (declare (ignore text))
        (is-true (field result "isError")
                 "id ~s did not produce a tool error" id)))))

(test a-run-belonging-to-another-account-is-not-readable
  (with-fixture caller ()
    (let* ((other (make-instance 'screenshotbot/model/company:company
                                 :name "someone else"))
           (channel (make-instance 'channel :name "theirs" :company other))
           (run (make-run other channel)))
      (multiple-value-bind (text result) (fetch-run-as token (oid run))
        (declare (ignore text))
        (is-true (field result "isError"))))))

(test fetching-a-run-without-the-scope-never-reaches-it
  (with-fixture caller ()
    (let* ((channel (add-channel "web"))
           (run (make-run company channel)))
      (multiple-value-bind (body status)
          (call-tool-as (token-with '("profile")) "fetch_run"
                        (list (cons "run_id" (oid run))))
        (is (equal 403 status))
        (is-false (str:containsp "screen-0" body))))))

(test a-truncated-screenshot-list-says-so
  "A run can carry more screenshots than a model can use, and one that
cannot see the cut reports a partial list as the whole one."
  (with-fixture caller ()
    (let* ((channel (add-channel "web"))
           (run (make-run company channel :screenshots 3)))
      (let ((json (progv (list '+max-screenshots+) (list 2)
                    (decode (fetch-run-as token (oid run))))))
        (is (equal 2 (length (field json "screenshots"))))
        (is (equal 3 (field json "screenshotsTotal")))))))

(test an-untruncated-run-says-nothing-about-truncation
  (with-fixture caller ()
    (let* ((channel (add-channel "web"))
           (run (make-run company channel :screenshots 2)))
      (let ((json (decode (fetch-run-as token (oid run)))))
        (is (equal 2 (length (field json "screenshots"))))
        (is-false (field json "screenshotsTotal"))))))

;; ----------------------------------------------------------------------
;; fetch_active_run
;; ----------------------------------------------------------------------

(test active-runs-are-reported-per-branch
  (with-fixture caller ()
    (let* ((channel (add-channel "web"))
           (main-run (make-run company channel))
           (release-run (make-run company channel)))
      (setf (active-run channel "main") main-run)
      (setf (active-run channel "release") release-run)
      (multiple-value-bind (text result) (fetch-active-run-as token "web")
        (is-false (field result "isError"))
        (let* ((json (decode text))
               (entries (field json "activeRuns")))
          (is (equal "web" (field json "channel")))
          (is (equal 2 (length entries)))
          ;; By branch rather than by position: nothing promises an order.
          (let ((by-branch (loop for entry in entries
                                 collect (cons (field entry "branch")
                                               (field (field entry "run") "id")))))
            (is (equal (oid main-run)
                       (cdr (assoc "main" by-branch :test #'equal))))
            (is (equal (oid release-run)
                       (cdr (assoc "release" by-branch :test #'equal))))))))))

(test active-runs-do-not-carry-screenshots
  "One entry per branch, each with a full screenshot list, is a lot of
output for a question the model asked to orient itself. It gets run ids
and calls fetch_run for the one it wants."
  (with-fixture caller ()
    (let* ((channel (add-channel "web"))
           (run (make-run company channel :screenshots 2)))
      (setf (active-run channel "main") run)
      (multiple-value-bind (text) (fetch-active-run-as token "web")
        (let* ((json (decode text))
               (entry (first (field json "activeRuns"))))
          (is-false (field (field entry "run") "screenshots"))
          ;; Still enough to go and fetch it.
          (is (equal (oid run) (field (field entry "run") "id"))))))))

(test a-channel-with-no-active-runs-gets-an-empty-list-not-an-error
  "A channel that has never promoted a run is a fact worth reporting. A
model handles [] fine; it handles a tool failure by giving up."
  (with-fixture caller ()
    (add-channel "web")
    (multiple-value-bind (text result) (fetch-active-run-as token "web")
      (is-false (field result "isError"))
      (is-false (field (decode text) "activeRuns")))))

(test an-unknown-channel-name-is-a-tool-error
  (with-fixture caller ()
    (multiple-value-bind (text result) (fetch-active-run-as token "no-such-channel")
      (declare (ignore text))
      (is-true (field result "isError")))))

(test another-accounts-channel-is-not-reachable-by-name
  "Channel lookup is scoped to the caller's company, so a name that exists
elsewhere has to read as simply absent."
  (with-fixture caller ()
    (let* ((other (make-instance 'screenshotbot/model/company:company
                                 :name "someone else"))
           (channel (make-instance 'channel :name "theirs" :company other))
           (run (make-run other channel)))
      (setf (active-run channel "main") run)
      (multiple-value-bind (text result) (fetch-active-run-as token "theirs")
        (is-true (field result "isError"))
        (is-false (str:containsp (oid run) text))))))

(test fetching-active-runs-without-the-scope-never-reaches-it
  (with-fixture caller ()
    (let* ((channel (add-channel "web"))
           (run (make-run company channel)))
      (setf (active-run channel "main") run)
      (multiple-value-bind (body status)
          (call-tool-as (token-with '("profile")) "fetch_active_run"
                        (list (cons "channel" "web")))
        (is (equal 403 status))
        (is-false (str:containsp (oid run) body))))))

;; ----------------------------------------------------------------------
;; promotion_logs_for_run
;; ----------------------------------------------------------------------

(defun promotion-logs-as (token id)
  (tool-text (call-tool-as token "promotion_logs_for_run"
                           (list (cons "run_id" id)))))

(defun write-promotion-log (run text)
  (let ((file (bknr.datastore:blob-pathname (promotion-log run))))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string text out))
    file))

(test a-promotion-log-is-returned-as-text-between-markers
  (with-fixture caller ()
    (let* ((channel (add-channel "web"))
           (run (make-run company channel)))
      (write-promotion-log run "Beginning promotion~%something happened")
      (multiple-value-bind (text result) (promotion-logs-as token (oid run))
        (is-false (field result "isError"))
        (is-true (str:containsp "something happened" text))
        (is-true (str:containsp "--BEGIN LOGS--" text))
        (is-true (str:containsp "--END LOGS--" text))))))

(test the-log-is-labelled-with-the-run-it-came-from
  "A debugging session gets several of these, and an unlabelled wall of
log is one a model will attribute to the wrong run."
  (with-fixture caller ()
    (let* ((channel (add-channel "web"))
           (run (make-run company channel)))
      (write-promotion-log run "hello")
      (let ((text (promotion-logs-as token (oid run))))
        (is-true (str:containsp (oid run) text))
        (is-true (str:containsp "web" text))
        (is-true (str:containsp "abc123" text))))))

(test a-run-that-was-never-promoted-says-so-and-is-not-an-error
  "The ordinary state of an unpromoted run, and itself the answer when the
question was why no report appeared."
  (with-fixture caller ()
    (let* ((channel (add-channel "web"))
           (run (make-run company channel)))
      (multiple-value-bind (text result) (promotion-logs-as token (oid run))
        (is-false (field result "isError"))
        (is-true (str:containsp "no promotion log" text))))))

(test a-long-log-keeps-the-end-and-says-what-it-dropped
  "A promotion that went wrong says so where it stopped, so the tail is
the half worth having."
  (with-fixture caller ()
    (let* ((channel (add-channel "web"))
           (run (make-run company channel)))
      (write-promotion-log run
                           (concatenate 'string
                                        (make-string 100 :initial-element #\a)
                                        "THE-INTERESTING-BIT"))
      (let ((text (progv (list '+max-log-characters+) (list 25)
                    (promotion-logs-as token (oid run)))))
        (is-true (str:containsp "THE-INTERESTING-BIT" text))
        (is-true (str:containsp "are not shown" text))
        ;; The head is gone, not merely unmentioned.
        (is-false (str:containsp (make-string 30 :initial-element #\a) text))))))

(test a-short-log-says-nothing-about-truncation
  (with-fixture caller ()
    (let* ((channel (add-channel "web"))
           (run (make-run company channel)))
      (write-promotion-log run "short")
      (is-false (str:containsp "are not shown"
                               (promotion-logs-as token (oid run)))))))

(test asking-for-an-unknown-runs-logs-is-a-tool-error
  (with-fixture caller ()
    (dolist (id (list "not-an-oid" "000000000000000000000000"))
      (multiple-value-bind (text result) (promotion-logs-as token id)
        (declare (ignore text))
        (is-true (field result "isError")
                 "id ~s did not produce a tool error" id)))))

(test another-accounts-promotion-log-is-not-readable
  "Promotion logs name commits, branches and build URLs."
  (with-fixture caller ()
    (let* ((other (make-instance 'screenshotbot/model/company:company
                                 :name "someone else"))
           (channel (make-instance 'channel :name "theirs" :company other))
           (run (make-run other channel)))
      (write-promotion-log run "their-secret-branch")
      (multiple-value-bind (text result) (promotion-logs-as token (oid run))
        (is-true (field result "isError"))
        (is-false (str:containsp "their-secret-branch" text))))))

(test reading-promotion-logs-without-the-scope-never-reaches-it
  (with-fixture caller ()
    (let* ((channel (add-channel "web"))
           (run (make-run company channel)))
      (write-promotion-log run "secret-log-content")
      (multiple-value-bind (body status)
          (call-tool-as (token-with '("profile")) "promotion_logs_for_run"
                        (list (cons "run_id" (oid run))))
        (is (equal 403 status))
        (is-false (str:containsp "secret-log-content" body))))))
