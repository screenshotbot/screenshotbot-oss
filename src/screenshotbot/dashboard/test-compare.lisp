(defpackage :screenshotbot/dashboard/test-compare
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/dashboard/compare
                #:report-result
                #:single-page-filter-options
                #:filter-groups-by-type
                #:group-matches-p
                #:info-modal
                #:sort-pixels
                #:pixel-str-to-components
                #:%render-commits-path
                #:find-commit-path-to-ancestor
                #:render-diff-report
                #:render-single-change-permalink
                #:metrics-page
                #:warmup-comparison-images-sync
                #:link-to-run
                #:random-non-alpha-px
                #:image-comparison-job
                #:prepare-image-comparison
                #:random-zoom-to-on-result
                #:same-density-style
                #:changed-rows-style
                #:find-changed-rows
                #:schedule-image-comparison
                #:changed-rows-check-response
                #:*changed-rows-pool*
                #:*scheduled-comparisons*
                #:change-image-row)
  (:import-from #:util/threading
                #:synchronous-executor)
  (:import-from #:screenshotbot/model/image-comparison
                #:find-image-comparison-from-cache
                #:image-comparison-changed-rows
                #:find-image-comparison-on-images
                #:transient-image-comparison)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/image
                #:dimension
                #:dimension-height
                #:image-dimensions
                #:make-image
                #:image
                #:local-image)
  (:import-from #:bknr.datastore
                #:store-object-id
                #:blob-pathname)
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot)
  (:import-from #:bknr.datastore
                #:delete-object)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:util/testing
                #:screenshot-static-page
                #:with-fake-request)
  (:import-from #:screenshotbot/report-api
                #:report)
  (:import-from #:screenshotbot/testing
                #:screenshot-test
                #:snap-all-images
                #:snap-image-blob
                #:with-test-user)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run
                #:recorder-run)
  (:import-from #:screenshotbot/dashboard/reports
                #:render-report-page)
  (:import-from #:screenshotbot/model/channel
                #:channel)
  (:import-from #:screenshotbot/installation
                #:installation
                #:*installation*)
  (:import-from #:screenshotbot/screenshot-api
                #:make-screenshot)
  (:import-from #:fiveam-matchers
                #:is-string
                #:has-typep
                #:assert-that)
  (:import-from #:fiveam-matchers/described-as
                #:described-as)
  (:import-from #:screenshotbot/magick/magick-lw
                #:with-pixel-wand
                #:pixel-set-color
                #:magick-new-image
                #:with-wand)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:fiveam-matchers/strings
                #:contains-string)
  (:import-from #:screenshotbot/diff-report
                #:all-groups
                #:renamed-group
                #:make-diff-report)
  (:import-from #:screenshotbot/model/screenshot-key
                #:screenshot-key)
  (:import-from #:screenshotbot/template
                #:app-template)
  (:import-from #:screenshotbot/model/figma
                #:update-figma-link)
  (:import-from #:screenshotbot/model/report
                #:base-acceptable)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:screenshotbot/user-api
                #:channel-repo)
  (:import-from #:screenshotbot/git-repo
                #:generic-git-repo
                #:commit-graph
                #:commit-graph-dag)
  (:import-from #:screenshotbot/model/commit-graph
                #:merge-dag-into-commit-graph
                #:find-or-create-commit-graph)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/dashboard/test-compare)


(util/fiveam:def-suite)

(def-fixture state ()
  (let ((*installation* (make-instance 'installation))
        ;; Rendering a report schedules image comparisons in the
        ;; background, which would otherwise outlive the test store.
        (*changed-rows-pool* (make-instance 'synchronous-executor)))
   (with-test-store ()
     (tmpdir:with-tmpdir (dir)
       (with-fake-request ()
         (let ((im1 #.(asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image.png"))
               (im2 #.(asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-2.png"))
               (im3 #.(asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-3.png"))
               (objs))
           (flet ((make-screenshot (img &key (name "foobar"))
                    (let* ((image (make-image :pathname img :for-tests t)))
                      (make-screenshot
                       :name name
                       :image image))))
             (&body))))))))


(test random-zoom-to-on-result
  (with-fixture state ()
    (let ((imc (make-instance 'transient-image-comparison
                               :result (make-screenshot im1))))
      (finishes (random-zoom-to-on-result imc nil))
      (let ((res
              (json:decode-json-from-string (random-zoom-to-on-result imc nil))))
        (is (eql 360 (a:assoc-value res :width)))
        (is (eql 360 (a:assoc-value res :height)))
        (is (< -1 (a:assoc-value res :x) 360))
        (is (< -1 (a:assoc-value res :y) 360))))))

(test metrics-page-happy-path
  "Since this uses FLI, we need to be extra careful about testing it."
  (with-fixture state ()
    (let ((imc (make-instance 'transient-image-comparison
                              :before (make-image :pathname im1 :for-tests t)
                              :after (make-image :pathname im2 :for-tests t)
                              :result (make-screenshot im1))))
      (finishes (metrics-page imc nil)))))

(test random-non-alpha-for-no-alphas
  (with-fixture state ()
    (with-wand (wand)
      (with-pixel-wand (pw)
        (pixel-set-color pw "none")
        (magick-new-image wand 10 10 pw)
        (multiple-value-bind (x y) (random-non-alpha-px wand nil)
          (is (eql -1 x))
          (is (eql -1 y)))))))

(def-fixture single-change ()
  (with-test-user (:user user
                     :company company
                     :logged-in-p t)
      (let* ((channel (make-instance 'channel
                                     :company company
                                     :name "bleh"
                                     :github-repo "git@github.com:a/b.gitq"))
             (one (make-recorder-run
                   :channel channel
                   :company company
                   :screenshots (list (make-screenshot im1))))
             (two (make-recorder-run
                   :channel channel
                   :company company
                   :screenshots (list (make-screenshot im2))))
             (report (make-instance 'report
                                    :title "foobar"
                                    :run one
                                    :previous-run two)))
        (&body))))

(test report-screenshot-test
  (with-fixture state ()
    (with-fixture single-change ()
      (snap-all-images)
      (screenshot-static-page
       :screenshotbot
       "report-page"
       (render-report-page report :skip-access-checks t)))))

(screenshot-test report-change-but-with-figma
  (with-fixture state ()
    (with-fixture single-change ()
      (update-figma-link :channel channel 
                   :screenshot-name "foobar"
                   :url "https://www.figma.com/file/ABC123/Screenshotbot-Report-UI?node-id=1%3A2"
                   :image (make-image :pathname im1 :for-tests t))
      (snap-all-images)
      (render-report-page report :skip-access-checks t))))

(test report-screenshot-test-with-one-screenshot-added
  (with-fixture state ()
    (with-test-user (:user user
                     :company company
                     :logged-in-p t)
      (let* ((channel (make-instance 'channel
                                     :company company
                                     :name "bleh"
                                     :github-repo "git@github.com:a/b.gitq"))
             (one (make-recorder-run
                   :channel channel
                   :company company
                   :screenshots (list (make-screenshot im1))))
             (two (make-recorder-run
                   :channel channel
                   :company company
                   :screenshots (list (make-screenshot im2 :name "renamed-screenshot"))))
             (report (make-instance 'report
                                    :title "foobar"
                                    :run one
                                    :previous-run two)))
        (snap-all-images)
        (screenshot-static-page
         :screenshotbot
         "report-page-with-one-screenshot-added"
         (render-report-page report :skip-access-checks t))))))


(test report-with-only-added-screenshots
  (with-fixture state ()
    (with-test-user (:user user
                     :company company
                     :logged-in-p t)
      (let* ((channel (make-instance 'channel
                                     :company company
                                     :name "bleh"
                                     :github-repo "git@github.com:a/b.gitq"))
             (one (make-recorder-run
                   :channel channel
                   :company company
                   :screenshots nil))
             (two (make-recorder-run
                   :channel channel
                   :company company
                   :screenshots (list (make-screenshot im2))))
             (report (make-instance 'report
                                    :title "foobar"
                                    :run one
                                    :previous-run two)))
        (snap-all-images)
        (screenshot-static-page
         :screenshotbot
         "report-page-only-added-screenshots"
         (render-report-page report :skip-access-checks t))))))

(test prepare-image-comparison
  (with-fixture state ()
    (with-test-user (:user user
                     :company company
                     :logged-in-p t)
      (let ((job (make-instance 'image-comparison-job
                                :before-image (make-screenshot im1)
                                :after-image (make-screenshot im2))))

        (let ((image-comparison (prepare-image-comparison job)))
          (assert-that image-comparison
                       (described-as "Expected to get a json response"
                           (is-string))))))))

(test link-to-empty-run
  (assert-that (format nil "~a" (link-to-run :run nil))
               (contains-string "empty run")))

(test warmup-comparison-images
  (with-fixture state ()
    (let ((run (make-recorder-run
                :screenshots (list (make-screenshot im1))))
          (to (make-recorder-run
               :screenshots (list (make-screenshot im2)))))
      (finishes
       (warmup-comparison-images-sync run to)))))

(screenshot-test single-change-permalink
  (with-fixture state ()
    (with-test-user (:user user
                     :company company
                     :logged-in-p t)
      (let* ((channel (make-instance 'channel
                                     :company company
                                     :name "bleh"
                                     :github-repo "git@github.com:a/b.gitq"))
             (one (make-recorder-run
                   :channel channel
                   :company company
                   :screenshots (list (make-screenshot im1))))
             (two (make-recorder-run
                   :channel channel
                   :company company
                   :screenshots (list (make-screenshot im2)))))
        (snap-all-images)
        (app-template
         :body-class "dashboard"
         (render-single-change-permalink
          (make-diff-report one two)
          (store-object-id (screenshot-key (make-screenshot im1)))
          "/report/dfdfd"
          :run one))))))

(screenshot-test single-added--permalink
  (with-fixture state ()
    (with-test-user (:user user
                     :company company
                     :logged-in-p t)
      (let* ((channel (make-instance 'channel
                                     :company company
                                     :name "bleh"
                                     :github-repo "git@github.com:a/b.gitq"))
             (one (make-recorder-run
                   :channel channel
                   :company company
                   :screenshots nil))
             (two (make-recorder-run
                   :channel channel
                   :company company
                   :screenshots (list (make-screenshot im2)))))
        (snap-all-images)
        (app-template
         :body-class "dashboard"
         (render-single-change-permalink
          (make-diff-report two one)
          (store-object-id (screenshot-key (make-screenshot im2)))
          "/report/dfdfd"
          :run one))))))

(screenshot-test comparison-with-acceptable-review-button
  (with-fixture state ()
    (with-test-user (:user user
                     :company company
                     :logged-in-p t)
      (let* ((channel (make-instance 'channel
                                     :company company
                                     :name "bleh"
                                     :github-repo "git@github.com:a/b.gitq"))
             (one (make-recorder-run
                   :channel channel
                   :company company
                   :screenshots (list (make-screenshot im1))))
             (two (make-recorder-run
                   :channel channel
                   :company company
                   :screenshots (list (make-screenshot im2))))
             (report (make-instance 'report
                                    :title "foobar"
                                    :channel channel
                                    :run one
                                    :previous-run two))
             (acceptable (make-instance 'base-acceptable
                                        :report report)))
        (snap-all-images)
        (app-template
         :body-class "dashboard"
         (render-diff-report
          :diff-report (make-diff-report one two)
          :acceptable acceptable
          :script-name "/report/test"))))))


(screenshot-test comparison-with-acceptable-review-button-with-review-button-clicked
  (with-fixture state ()
    (with-test-user (:user user
                     :company company
                     :logged-in-p t)
      (let* ((channel (make-instance 'channel
                                     :company company
                                     :name "bleh"
                                     :github-repo "git@github.com:a/b.gitq"))
             (one (make-recorder-run
                   :channel channel
                   :company company
                   :screenshots (list (make-screenshot im1))))
             (two (make-recorder-run
                   :channel channel
                   :company company
                   :screenshots (list (make-screenshot im2))))
             (report (make-instance 'report
                                    :title "foobar"
                                    :channel channel
                                    :run one
                                    :previous-run two))
             (acceptable (make-instance 'base-acceptable
                                        :report report)))
        (snap-all-images)
        (expand-review-button
         (app-template
          :body-class "dashboard"
          (render-diff-report
           :diff-report (make-diff-report one two)
           :acceptable acceptable
           :script-name "/report/test")))))))

(defun expand-review-button (html)
  (mquery:with-document (html)
    (let ((dropdown-button (mquery:$ "[id='reviewDropdownMenuButton']")))
      (assert-that dropdown-button (has-length 1))
      (mquery:add-class dropdown-button "show")
      (let ((dropdown-menu (mquery:$ ".review-dropdown-menu")))
        (assert-that dropdown-menu (has-length 1))
        (mquery:add-class dropdown-menu "show")))))

(screenshot-test review-button-dropdown-when-not-logged-in
  (with-fixture state ()
    (with-test-user (:user user
                     :company company
                     :logged-in-p nil)
      (with-fake-request ()
        (auth:with-sessions ()
          (let* ((channel (make-instance 'channel
                                         :company company
                                         :name "bleh"
                                         :github-repo "git@github.com:a/b.gitq"))
                 (one (make-recorder-run
                       :channel channel
                       :company company
                       :screenshots (list (make-screenshot im1))))
                 (two (make-recorder-run
                       :channel channel
                       :company company
                       :screenshots (list (make-screenshot im2))))
                 (report (make-instance 'report
                                        :title "foobar"
                                        :channel channel
                                        :run one
                                        :previous-run two))
                 (acceptable (make-instance 'base-acceptable
                                            :report report)))
            (snap-all-images)
            (expand-review-button
             (app-template
              :body-class "dashboard"
              (render-diff-report
               :diff-report (make-diff-report one two)
               :acceptable acceptable
               :script-name "/report/test")))))))))

(test find-commit-path-to-ancestor-happy-path
  (with-fixture state ()
    (with-test-user (:user user
                     :company company
                     :logged-in-p t)
      (let* ((repo-url "https://github.com/test/repo")
             ;; Create commit hashes (40 hex chars)
             (commit-a "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
             (commit-b "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
             (commit-c "cccccccccccccccccccccccccccccccccccccccc")
             ;; Set up the commit graph with a DAG: A -> B -> C
             (cg (find-or-create-commit-graph company repo-url))
             (dag (make-instance 'dag:dag)))
        ;; Add commits: A has parent B, B has parent C, C is root
        (dag:add-commit dag (make-instance 'dag:commit
                                           :sha commit-c
                                           :parents nil))
        (dag:add-commit dag (make-instance 'dag:commit
                                           :sha commit-b
                                           :parents (list commit-c)))
        (dag:add-commit dag (make-instance 'dag:commit
                                           :sha commit-a
                                           :parents (list commit-b)))
        (merge-dag-into-commit-graph cg dag)
        ;; Create channel with the repo URL
        (let* ((channel (make-instance 'channel
                                       :company company
                                       :name "test-channel"
                                       :github-repo repo-url))
               ;; Create runs with commits
               (run (make-recorder-run
                     :channel channel
                     :company company
                     :commit-hash commit-a))
               (to (make-recorder-run
                    :channel channel
                    :company company
                    :commit-hash commit-c)))
          ;; Test find-commit-path-to-ancestor
          (multiple-value-bind (path this-hash prev-hash)
              (find-commit-path-to-ancestor run to)
            (is (equal (list commit-a commit-b commit-c) path))
            (is (equal commit-a this-hash))
            (is (equal commit-c prev-hash))))))))

(test find-commit-path-to-ancestor-when-no-commit-hashes
  (with-fixture state ()
    (with-test-user (:user user
                     :company company
                     :logged-in-p t)
      ;; Create channel with the repo URL
      (let* ((channel (make-instance 'channel
                                     :company company
                                     :name "test-channel"))
             ;; Create runs with commits
             (run (make-recorder-run
                   :channel channel
                   :company company))
             (to (make-recorder-run
                  :channel channel
                  :company company)))
        ;; Test find-commit-path-to-ancestor
        (multiple-value-bind (path this-hash prev-hash)
            (find-commit-path-to-ancestor run to)
          (is (equal nil path)))))))

(test find-commit-path-when-{to}-does-not-exist
  (with-fixture state ()
    (with-test-user (:user user
                     :company company
                     :logged-in-p t)
      (let* ((repo-url "https://github.com/test/repo"))
        ;; Create channel with the repo URL
        (let* ((channel (make-instance 'channel
                                       :company company
                                       :name "test-channel"
                                       :github-repo repo-url))
               ;; Create runs with commits
               (run (make-recorder-run
                     :channel channel
                     :company company)))
          ;; Test find-commit-path-to-ancestor
          (multiple-value-bind (path this-hash prev-hash)
              (find-commit-path-to-ancestor run nil)
            (is (equal nil path))))))))

(screenshot-test render-commits-path
  (with-fixture state ()
   (let ((repo (make-instance 'generic-git-repo :link "https://github.com/tdrhq/fast-example"))
         (commit-a "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
         (commit-b "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
         (commit-c "cccccccccccccccccccccccccccccccccccccccc"))
     (%render-commits-path
      repo
      (list commit-a commit-b commit-c)
      commit-a
      commit-c))))

(screenshot-test could-not-find-commit-path
  (with-fixture state ()
   (let ((repo (make-instance 'generic-git-repo :link "https://github.com/tdrhq/fast-example"))
         (commit-a "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
         (commit-c "cccccccccccccccccccccccccccccccccccccccc"))
     (%render-commits-path
      repo
      nil
      commit-a
      commit-c))))




(test pixel-str-to-components
  (is (equal '(10 20 30)
             (pixel-str-to-components
              "srgb(10,20,30)"))))

(test sort-pixels
  (is
   (equalp
    '(2 0 1)
    (sort-pixels
     #2A((1 2) (3 4) (5 6))
     '("srgb(243,246,248)"
       "srgb(242,244,247)"
       "srgb(242,245,248)")
     '("srgb(243,245,248)"
       "srgb(242,245,247)"
       "srgb(242,245,246)")))))

(test info-modal-when-theres-no-TO-provided
  (with-fixture state ()
   (with-fixture single-change ()
     (finishes
       (info-modal one nil)))))

(test same-density-style
  (is (equal "flex: 0 1 min(400px, calc(80vh * 1.333333)); min-width: 0;"
             (same-density-style (make-instance 'dimension :width 400 :height 300)
                                 300)))
  (is (equal "flex: 0 1 min(300px, calc(80vh * 1.000000)); min-width: 0;"
             (same-density-style (make-instance 'dimension :width 300 :height 200)
                                 300))))

(test changed-rows-style
  (is (equal "--changed-rows-fraction: 0.500000;"
             (changed-rows-style (make-instance 'dimension :width 1 :height 150)
                                 300))))

(def-fixture changed-rows ()
  (let ((company (make-instance 'company)))
    (let ((before (make-image :pathname im1 :company company))
          (after (make-image :pathname im3 :company company)))
      (&body))))

(defun make-comparison-without-strip (before after)
  "A comparison that was computed without a strip, e.g. before we
started computing them."
  (let ((comparison (find-image-comparison-on-images before after)))
    (setf (image-comparison-changed-rows comparison) nil)
    comparison))

(test find-changed-rows-schedules-the-comparison
  (with-fixture state ()
    (with-fixture changed-rows ()
      (let ((*changed-rows-pool* (make-instance 'synchronous-executor))
            (*scheduled-comparisons* (fset:empty-set)))
        ;; The first render doesn't show the strip, but schedules the
        ;; comparison (run immediately here), so the strip shows up
        ;; the next time.
        (multiple-value-bind (changed-rows pending-p)
            (find-changed-rows before after)
          (is (equal nil changed-rows))
          (is-true pending-p))
        (is-true (find-image-comparison-from-cache :before before :after after))
        (multiple-value-bind (changed-rows pending-p)
            (find-changed-rows before after)
          (is-true changed-rows)
          (is-false pending-p))
        (is (fset:empty? *scheduled-comparisons*))))))

(test find-changed-rows-schedules-the-strip-for-a-comparison-without-one
  (with-fixture state ()
    (with-fixture changed-rows ()
      (let ((comparison (make-comparison-without-strip before after))
            (*changed-rows-pool* (make-instance 'synchronous-executor))
            (*scheduled-comparisons* (fset:empty-set)))
        (multiple-value-bind (changed-rows pending-p)
            (find-changed-rows before after)
          (is (equal nil changed-rows))
          (is-true pending-p))
        (is-true (image-comparison-changed-rows comparison))
        (multiple-value-bind (changed-rows pending-p)
            (find-changed-rows before after)
          (is (eql (image-comparison-changed-rows comparison) changed-rows))
          (is-false pending-p))))))

(test schedule-image-comparison-skips-already-scheduled-comparisons
  (with-fixture state ()
    (with-fixture changed-rows ()
      (let ((*changed-rows-pool* (make-instance 'synchronous-executor))
            (*scheduled-comparisons* (fset:set (cons (store-object-id before)
                                                     (store-object-id after)))))
        ;; In the other order, since the comparison doesn't depend on it.
        (schedule-image-comparison after before)
        (is (equal nil (find-image-comparison-from-cache :before before :after after)))))))

(test find-changed-rows-with-a-cached-comparison
  (with-fixture state ()
    (with-fixture changed-rows ()
      (let ((comparison (find-image-comparison-on-images before after)))
        (is-true (image-comparison-changed-rows comparison))
        (is (eql (image-comparison-changed-rows comparison)
                 (find-changed-rows before after)))))))

(defun max-height (before after)
  (max (dimension-height (image-dimensions before))
       (dimension-height (image-dimensions after))))

(test changed-rows-check-response-before-the-comparison-is-ready
  (with-fixture state ()
    (with-fixture changed-rows ()
      (is (equal `((:state . "pending"))
                 (changed-rows-check-response before after (max-height before after)))))))

(test changed-rows-check-response-when-the-strip-is-ready
  (with-fixture state ()
    (with-fixture changed-rows ()
      (find-image-comparison-on-images before after)
      (let ((response (changed-rows-check-response before after (max-height before after))))
        (is (equal "ready" (assoc-value response :state)))
        (assert-that (assoc-value response :data)
                     (contains-string "changed-rows-strip"))))))

(test changed-rows-check-response-strip-opens-the-comparison-modal
  (with-fixture state ()
    (with-fixture changed-rows ()
      (find-image-comparison-on-images before after)
      (let ((response (changed-rows-check-response before after (max-height before after)
                                                   :toggle-id "toggle-id-3")))
        (assert-that (assoc-value response :data)
                     (contains-string "data-bs-toggle=\"modal\"")
                     (contains-string "data-bs-target=\"#toggle-id-3\""))))))

(test changed-rows-check-response-while-the-strip-is-scheduled
  (with-fixture state ()
    ;; The comparison exists without a strip, but we've scheduled
    ;; computing one, so keep checking.
    (with-fixture changed-rows ()
      (make-comparison-without-strip before after)
      (let ((*scheduled-comparisons* (fset:set (cons (store-object-id before)
                                                     (store-object-id after)))))
        (is (equal `((:state . "pending"))
                   (changed-rows-check-response before after (max-height before after))))))))

(test changed-rows-check-response-when-the-comparison-has-no-strip
  (with-fixture state ()
    (with-fixture changed-rows ()
      (make-comparison-without-strip before after)
      (is (equal `((:state . "none"))
                 (changed-rows-check-response before after (max-height before after)))))))

(defun render-change-image-row (before after &key changed-rows changed-rows-pending-p
                                                  toggle-id)
  (markup:write-html
   (change-image-row :before-image before
                     :after-image after
                     :before-dims (image-dimensions before)
                     :after-dims (image-dimensions after)
                     :changed-rows changed-rows
                     :changed-rows-pending-p changed-rows-pending-p
                     :toggle-id toggle-id)))

(test change-image-row-shows-the-arrow-without-changed-rows
  (with-fixture state ()
    (with-fixture changed-rows ()
      (let ((html (render-change-image-row before after)))
        (assert-that html
                     (contains-string "arrow-forward"))
        (is-false (search "changed-rows-strip" html))
        (is-false (search "changed-rows-pending" html))))))

(test change-image-row-checks-for-the-strip-if-its-pending
  (with-fixture state ()
    (with-fixture changed-rows ()
      (auth:with-sessions ()
        (let ((html (render-change-image-row before after
                                             :changed-rows-pending-p t)))
          (assert-that html
                       (contains-string "arrow-forward changed-rows-pending")
                       (contains-string "data-changed-rows-check="))
          (is-false (search "changed-rows-strip" html)))))))

(test change-image-row-shows-the-changed-rows-strip
  (with-fixture state ()
    (with-fixture changed-rows ()
      (let* ((changed-rows (image-comparison-changed-rows
                            (find-image-comparison-on-images before after)))
             (html (render-change-image-row before after
                                            :changed-rows changed-rows)))
        (assert-that html
                     (contains-string "changed-rows-strip")
                     (contains-string (changed-rows-style
                                       (image-dimensions changed-rows)
                                       (max-height before after))))
        (is-false (search "arrow-forward" html))))))

(test change-image-row-strip-opens-the-comparison-modal
  (with-fixture state ()
    (with-fixture changed-rows ()
      (let* ((changed-rows (image-comparison-changed-rows
                            (find-image-comparison-on-images before after)))
             (html (render-change-image-row before after
                                            :changed-rows changed-rows
                                            :toggle-id "toggle-id-3")))
        (assert-that html
                     (contains-string "data-bs-toggle=\"modal\"")
                     (contains-string "data-bs-target=\"#toggle-id-3\""))))))

(test change-image-row-strip-without-a-modal
  (with-fixture state ()
    (with-fixture changed-rows ()
      (let* ((changed-rows (image-comparison-changed-rows
                            (find-image-comparison-on-images before after)))
             (html (render-change-image-row before after
                                            :changed-rows changed-rows)))
        (assert-that html
                     (contains-string "changed-rows-strip"))
        (is-false (search "data-bs-toggle" html))))))

(def-fixture single-page-comparison ()
  (with-test-user (:user user
                   :company company
                   :logged-in-p t)
    (let* ((channel (make-instance 'channel
                                   :company company
                                   :name "bleh"
                                   :github-repo "git@github.com:a/b.gitq"))
           (one (make-recorder-run
                 :channel channel
                 :company company
                 :screenshots (list (make-screenshot im1)
                                    (make-screenshot im3 :name "added-one"))))
           (two (make-recorder-run
                 :channel channel
                 :company company
                 :screenshots (list (make-screenshot im2)
                                    (make-screenshot im2 :name "deleted-one")))))
      (flet ((render ()
               (render-diff-report
                :diff-report (make-diff-report one two)
                :script-name "/report/test")))
        (&body)))))

(defun render-to-string (html)
  (markup:write-html html))

(test single-page-comparison-is-on-by-default
  (with-fixture state ()
    (with-fixture single-page-comparison ()
      (let ((html (render-to-string (render))))
        (assert-that html
                     (contains-string "single-page-filter"))
        (is (not (search "<a class=\"nav-link" html)))))))

(test single-page-comparison-with-no-changes
  (with-fixture state ()
    (with-fixture single-page-comparison ()
      (let ((html (render-to-string
                   (render-diff-report
                    :diff-report (make-diff-report one one)
                    :script-name "/report/test"))))
        (assert-that html
                     (contains-string "No changes match filters"))
        ;; With no groups, the filter is just a plain All.
        (is (not (search "data-type=\"changes\"" html)))))))

(test single-page-comparison-can-be-turned-off
  (with-fixture state ()
    (with-fixture single-page-comparison ()
      (gk:create :single-page-comparison :default nil)
      (let ((html (render-to-string (render))))
        (assert-that html
                     (contains-string "data-type=\"added\""))
        (is (not (search "badge bg-success" html)))
        (is (not (search "border-success" html)))))))

(test single-page-comparison-renders-all-groups-in-order
  (with-fixture state ()
    (with-fixture single-page-comparison ()
      (gk:create :single-page-comparison)
      (gk:allow :single-page-comparison company)
      (let ((html (render-to-string (render))))
        ;; The tabs are gone (the filter dropdown has data-types too).
        (is (not (search "<a class=\"nav-link" html)))
        (let ((added (search "added-one" html))
              (deleted (search "deleted-one" html))
              (changed (search "<span class=\"index\">3</span> foobar" html)))
          (is-true added)
          (is-true deleted)
          (is-true changed)
          (is (< added deleted changed)))
        (assert-that html
                     (contains-string "badge bg-success")
                     (contains-string "badge bg-danger")
                     (contains-string "card mb-3 border-success")
                     (contains-string "card mb-3 border-danger"))
        (is (not (search "col-md-6" html)))))))

(def-fixture single-page-rename ()
  (with-test-user (:company company
                   :logged-in-p t)
    (gk:create :single-page-comparison)
    (gk:allow :single-page-comparison company)
    (let* ((channel (make-instance 'channel
                                   :company company
                                   :name "bleh"))
           (one (make-recorder-run
                 :channel channel
                 :company company
                 :screenshots (list (make-screenshot im3 :name "new-name"))))
           (two (make-recorder-run
                 :channel channel
                 :company company
                 :screenshots (list (make-screenshot im3 :name "old-name"))))
           (diff-report (make-diff-report one two)))
      (flet ((render ()
               (render-diff-report
                :diff-report diff-report
                :script-name "/report/test")))
        (&body)))))

(test single-page-comparison-renders-a-rename-as-one-card
  (with-fixture state ()
    (with-fixture single-page-rename ()
      (let ((html (render-to-string (render))))
        (assert-that html
                     (contains-string "badge bg-warning"))
        (is (not (search "badge bg-success" html)))
        (is (not (search "badge bg-danger" html)))
        (is (not (search "<span class=\"index\">2</span>" html)))
        (is (not (search "border-success" html)))
        (is (not (search "border-danger" html)))
        (let ((old (search "old-name" html))
              (new (search "new-name" html)))
          (is-true old)
          (is-true new)
          (is (< old new)))
        (let ((renamed (fset:arb (all-groups diff-report))))
          (is (typep renamed 'renamed-group))
          (is-true (group-matches-p renamed "old"))
          (is-true (group-matches-p renamed "new"))
          (is-false (group-matches-p renamed "unrelated")))))))

(screenshot-test single-page-comparison-with-a-rename
  (with-fixture state ()
    (with-fixture single-page-rename ()
      (snap-all-images)
      (app-template
       :body-class "dashboard"
       (render)))))

(test single-page-filter-counts-each-type
  (with-fixture state ()
    (with-fixture single-page-comparison ()
      (is (equal
           '(("all" . "All (1 changed, 1 added, 1 deleted)")
             ("changes" . "1 changed")
             ("added" . "1 added")
             ("deleted" . "1 deleted"))
           (single-page-filter-options
            (fset:convert 'list (all-groups (make-diff-report one two)))))))))

(test single-page-filter-counts-renames-separately
  (with-fixture state ()
    (with-fixture single-page-rename ()
      (is (equal "All (1 renamed)"
                 (cdr (assoc "all"
                             (single-page-filter-options
                              (fset:convert 'list (all-groups diff-report)))
                             :test #'string=)))))))

(test filter-groups-by-type
  (with-fixture state ()
    (with-fixture single-page-comparison ()
      (let ((groups (fset:convert 'list (all-groups (make-diff-report one two)))))
        (flet ((titles (type)
                 (mapcar #'screenshotbot/diff-report:group-title
                         (filter-groups-by-type groups type))))
          (is (equal '("added-one") (titles "added")))
          (is (equal '("deleted-one") (titles "deleted")))
          (is (equal '("foobar") (titles "changes")))
          (is (equal nil (titles "renamed")))
          (is (equal (titles "all") (titles nil)))
          (assert-that (titles "all")
                       (has-length 3)))))))

(test single-page-comparison-renders-the-filter-dropdown
  (with-fixture state ()
    (with-fixture single-page-comparison ()
      (gk:create :single-page-comparison)
      (gk:allow :single-page-comparison company)
      (let ((html (render-to-string (render))))
        (assert-that html
                     (contains-string "single-page-filter")
                     ;; The counts next to All are hidden on small screens.
                     (contains-string "All<span class=\"d-none d-md-inline\"> (1 changed, 1 added, 1 deleted)</span>")
                     (contains-string "data-type=\"deleted\""))
        ;; There are no renames, so there's no option for them.
        (is (not (search "data-type=\"renamed\"" html)))))))

(test single-page-report-result-filters-by-type
  (with-fixture state ()
    (with-fixture single-page-comparison ()
      (let* ((diff-report (make-diff-report one two))
             (html (render-to-string
                    (report-result one nil nil nil
                                   :type "added"
                                   :single-page-p t
                                   :all-groups (fset:convert 'list (all-groups diff-report))
                                   :script-name "/report/test"))))
        (assert-that html
                     (contains-string "added-one"))
        (is (not (search "deleted-one" html)))
        (is (not (search "foobar" html)))))))

(test single-page-filter-with-no-groups-is-just-all
  (is (equal '(("all" . "All"))
             (single-page-filter-options nil))))

(test single-page-comparison-puts-search-after-the-filter
  (with-fixture state ()
    (with-fixture single-page-comparison ()
      (let* ((html (render-to-string (render)))
             (search (search "search-toggle" html)))
        (is-true search)
        ;; Only one search box: none next to the channel name.
        (is (= search (search "search-toggle" html :from-end t)))
        (is (< (search "single-page-filter" html) search))
        (is (< search (search "btn btn-secondary dropdown-toggle" html)))))))

(test tabbed-comparison-keeps-search-next-to-the-channel-name
  (with-fixture state ()
    (with-fixture single-page-comparison ()
      (gk:create :single-page-comparison :default nil)
      (let* ((html (render-to-string (render)))
             (search (search "search-toggle" html)))
        (is-true search)
        (is (< (search "channel-search-bar-name" html) search))
        (is (< search (search "data-type=\"changes\"" html)))))))

(screenshot-test single-page-comparison
  (with-fixture state ()
    (with-fixture single-page-comparison ()
      (gk:create :single-page-comparison)
      (gk:allow :single-page-comparison company)
      (snap-all-images)
      (app-template
       :body-class "dashboard"
       (render)))))
