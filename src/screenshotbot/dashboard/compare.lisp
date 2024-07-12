;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/dashboard/compare
  (:nicknames #:screenshotbot/compare #|For bknr|#)
  (:use #:cl
        #:alexandria
        #:nibble
        #:screenshotbot/template
        #:screenshotbot/model/screenshot
        #:screenshotbot/model/image-comparison
        #:screenshotbot/model/image
        #:screenshotbot/model/view
        #:screenshotbot/model/report
        #:screenshotbot/model/channel
        #:screenshotbot/ignore-and-log-errors
        #:screenshotbot/model/recorder-run)
  (:import-from #:util
                #:find-by-oid
                #:oid)
  (:import-from #:markup #:deftag)
  (:import-from #:screenshotbot/server
                #:make-thread
                #:defhandler)
  (:import-from #:screenshotbot/report-api
                #:render-diff-report)
  (:import-from #:screenshotbot/dashboard/run-page
                #:render-run-tags
                #:modal-id
                #:render-modal
                #:screenshots-viewer
                #:run-row-filter
                #:page-nav-dropdown
                #:row-filter
                #:mask-editor
                #:commit)
  (:import-from #:screenshotbot/model/image
                #:dimension-width
                #:dimension-height
                #:image-dimensions
                #:map-unequal-pixels
                #:image-blob
                #:rect-as-list)
  (:import-from #:core/ui/paginated
                #:paginated)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:bknr.datastore
                #:make-blob-from-file)
  (:import-from #:screenshotbot/user-api
                #:adminp
                #:can-view!
                #:current-user
                #:created-at
                #:current-company)
  (:import-from #:screenshotbot/dashboard/image
                #:handle-resized-image)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:auto-restart
                #:with-auto-restart)
  (:import-from #:bknr.datastore
                #:delete-object)
  (:import-from #:nibble
                #:nibble-url)
  (:import-from #:screenshotbot/magick/magick-lw
                #:get-px-as-string
                #:magick-get-image-width
                #:magick-get-image-height
                #:get-non-alpha-pixels
                #:with-wand)
  (:import-from #:screenshotbot/diff-report
                #:group-renamed-p
                #:group
                #:deleted-groups
                #:added-groups
                #:diff-report-changes
                #:make-diff-report
                #:actual-item
                #:changes-groups
                #:get-tab-title)
  (:import-from #:bknr.datastore
                #:cascading-delete-object)
  (:import-from #:core/ui/taskie
                #:timeago)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:screenshotbot/model/transient-object
                #:with-transient-copy)
  (:import-from #:screenshotbot/model/image-comparison
                #:image-comparison-before
                #:find-image-comparison-on-images)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:screenshotbot/model/view
                #:can-edit
                #:can-edit!)
  (:import-from #:screenshotbot/dashboard/review-link
                #:review-link)
  (:import-from #:screenshotbot/cdn
                #:make-image-cdn-url)
  (:import-from #:screenshotbot/model/screenshot
                #:abstract-screenshot)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-tags
                #:run-build-url)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:screenshotbot/model/review-policy
                #:can-review?)
  (:import-from #:screenshotbot/model/channel
                #:review-policy)
  (:import-from #:screenshotbot/model/report
                #:report-channel)
  (:export
   #:render-acceptable
   #:render-diff-report
   #:warmup-comparison-images)
  (:local-nicknames (#:diff-report #:screenshotbot/diff-report)))
(in-package :screenshotbot/dashboard/compare)

;; fake symbols for bknr migration
(progn
 '(result
   before
   after
   identical-p
   result))

(defvar *summarizer* nil)

(named-readtables:in-readtable markup:syntax)

(defhandler (acceptable-review-url :uri "/acceptable/:id/review" :method :post)
            (id action redirect csrf)
  (assert (equal csrf (auth:csrf-token)))
  (let ((acceptable (bknr.datastore:store-object-with-id (parse-integer id))))
    (can-edit! acceptable)
    (let ((state (cond
                   ((string-equal "accept" action)
                    :accepted)
                   ((string-equal "reject" action)
                    :rejected)
                   (t
                    (error "Unknown acceptable review action: ~a" action)))))
      (setf (acceptable-state acceptable :user (current-user))
            state)
      (hex:safe-redirect redirect))))

(deftag render-acceptable (&key acceptable)
  (let ((review-url (hex:make-url 'acceptable-review-url :id (bknr.datastore:store-object-id
                                                              acceptable)))
        (btn-class
          (ecase (acceptable-state acceptable)
            (:accepted
             "dropdown-report-accepted")
            (:rejected
             "dropdown-report-rejected")
            ((nil)
             "")))
        (btn-text
          (ecase (acceptable-state acceptable)
            (:accepted
             "Accepted")
            (:rejected
             "Rejected")
            ((nil)
             "Review"))))
    (flet ((form-menu (&key title action)
             <form action=review-url method= "POST" >
               <button action= "submit" class= (format nil "btn btn-link acceptable ~a-link dropdown-item" action) >
                 <input type= "hidden" name= "action" value= action />
                 <input type= "hidden" name= "csrf" value= (auth:csrf-token) />
                 <input type= "hidden" name= "redirect"
                        value= (hunchentoot:script-name*) />
                 <mdi name= "check" />
                 ,(progn title)
               </button>
             </form>))
      <markup:merge-tag>
        <button class= (format nil"btn  btn-secondary dropdown-toggle ~a" btn-class) type="button" id="dropdownMenuButton" data-bs-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
          ,(progn btn-text)
        </button>

        ,(let ((channel (report-channel (acceptable-report acceptable))))
           (cond
             ((can-review? (review-policy channel)
                           (acceptable-report acceptable)
                           (auth:current-user))
              <div class="dropdown-menu" aria-labelledby="dropdownMenuButton" style= "z-index: 99999999; position: static" >
                ,(form-menu :title "Accept" :action "accept")
                ,(form-menu :title "Reject" :action "reject")
              </div>)
             (t
              <div class="dropdown-menu p-4 text-muted" style="max-width: 200px;">
                <p class= "mb-0" >
                  The <a href= (format nil "/channels/~a#review-policy" (store-object-id channel))>review policy</a> for this channel does not let authors review their own screenshots.
                </p>
              </div>)))
      </markup:merge-tag>)))

(defhandler (compare-page :uri "/runs/:id/compare/:to") (id to)
  (when (string= "254" id)
    (hex:safe-redirect "/report/5fd16bcf4f4b3822fd000146"))
  (flet ((find-run (id)
           (let ((ret (find-by-oid id 'recorder-run)))
             (assert ret)
             ret)))
   (let* ((run (find-run id))
          (to (find-run to)))
     (can-view! run to)
     <app-template body-class= "dashboard bg-white" >
     ,(async-diff-report :run run :to to)
     </app-template>)))

(deftag picture-with-img (&key image dimensions alt class)
  <picture>
    <source srcset= (image-public-url image :size :full-page :type :webp) type= "image/webp" />
    <img class= (format nil "screenshot-image change-image ~a" class) src= (image-public-url image :size :full-page :type :png)
         loading= "lazy"
         alt=alt
         width= (?. dimension-width dimensions)
         height= (?. dimension-height dimensions)
/>

  </picture>)

(deftag change-image-row (&key before-image
                          after-image
                          before-dims
                          after-dims)
  <div class="change-image-row">

    <picture-with-img
      image=before-image
      dimensions=before-dims
      alt= "before image"
      class= "change-image-left" />
    <mdi name= "arrow_forward" />

    <picture-with-img
      image=after-image
      dimensions=after-dims
      alt="after image"
      class= "change-image-right" />

  </div>)

(deftag change-image-row-triple (&key before-image
                          after-image
                          comp-image)
  <div class="change-image-row change-image-row-triple">
    <img class= "screenshot-image change-image change-image-left" src= before-image />
    <img class= "screenshot-image change-image change-image-right" src= after-image />
    <:img data-src= comp-image
      class= "bg-primary image-comparison-modal-image screenshot-image change-image" alt= "Image Difference" />
  </div>)

(defclass image-comparison-job ()
  ((donep :initform nil
          :accessor donep)
   (lock :initform (bt:make-lock "image-comparison")
         :reader lock)
   (before-image :initarg :before-image
                 :reader before-image)
   (after-image :initarg :after-image
                :reader after-image)
   (image-comparison
    :initarg :image-comparison
    :accessor image-comparison
    :documentation "The actual image-comparison object. You can get
    the resulting image and other context from this object")))


(defmethod find-image-comparison ((before-screenshot abstract-screenshot)
                                  (after-screenshot abstract-screenshot))
  ;; second level of caching, we're going to look through the
  ;; datastore to see if there are any previous images
  (let ((before (screenshot-image before-screenshot))
        (after (screenshot-image after-screenshot)))
    ;; Avoid computation for large reverts
    (when (> (store-object-id before)
             (store-object-id after))
      (rotatef before after))
    (find-image-comparison-on-images
     before
     after)))

(defmethod prepare-image-comparison-file ((self image-comparison-job))
  (bt:with-lock-held ((lock self))
    (cond
      ((donep self)
       (image-comparison self))
      (t
       (setf (image-comparison self)
             (find-image-comparison (before-image self)
                                    (after-image self)))))))

(defmethod prepare-image-comparison ((self image-comparison-job)
                                     &key
                                       ;; I can't use :full-page here because the JS isn't
                                       ;; designed to handle that yet.
                                       (size nil)
                                       (warmup nil))
  (let ((image-comparison (prepare-image-comparison-file self)))
    (cond
      (warmup
       (when size
        (handle-resized-image (image-comparison-result image-comparison) size :warmup t)))
      (t
       (setf (hunchentoot:content-type*) "application/json")
       (let ((masks (screenshot-masks (after-image self))))
        (json:encode-json-to-string
         `((:identical . ,(identical-p image-comparison))
           ;; for debugging: e.g. if we need to delete the comparison
           (:store-object-id
            .
            ,(when (typep image-comparison 'store-object)
               (bknr.datastore:store-object-id image-comparison)))
           (:zoom-to . ,(nibble-url (nibble (:name :zoom) (random-zoom-to-on-result
                                                           image-comparison masks))))
           (:metrics . ,(nibble-url (nibble (:name :metrics)
                                      (metrics-page image-comparison masks))))
           (:src . ,(make-image-cdn-url (image-public-url (image-comparison-result image-comparison) :size size)))
           (:background . ,(make-image-cdn-url (image-public-url (screenshot-image (before-image self)) :size size)))
           (:after-image . ,(make-image-cdn-url (image-public-url (screenshot-image (after-image self)) :size size)))
           (:masks .
                   ,(or
                     (loop for mask in masks
                           collect
                           `((:left . ,(mask-rect-left mask))
                             (:top . ,(mask-rect-top mask))
                             (:width . ,(mask-rect-width mask))
                             (:height . ,(mask-rect-height mask))))
                     #())))))))))

(defun random-non-alpha-px (wand masks)
  (let ((pxs (get-non-alpha-pixels wand
                                   :masks masks)))
    (let ((num (car (array-dimensions pxs))))
     (cond
       ((= num 0)
        (values -1 -1))
       (t
        (let ((i (random num)))
          (values
           (aref pxs i 0)
           (aref pxs i 1))))))))

(defun random-zoom-to-on-result (image-comparison masks)
  (setf (hunchentoot:content-type*) "application/json")
  (with-local-image (file (image-comparison-result image-comparison))
    (with-wand (wand :file file)
      (log:debug"random-zoom-to-on-result on ~a" file)
      (multiple-value-bind (x y) (random-non-alpha-px wand masks)
        (let ((dims (image-dimensions (image-comparison-result image-comparison))))
          (json:encode-json-to-string
           `((:y . ,y)
             (:x . ,x)
             (:width . ,(dimension-width dims))
             (:height . ,(dimension-height dims)))))))))


(defun async-diff-report (&rest args &key &allow-other-keys)
  (let* ((data nil)
         (session auth:*current-session*)
         (request hunchentoot:*request*)
         (acceptor hunchentoot:*acceptor*)
         (data-check-nibble (nibble (:name :data-check)
                              (setf (hunchentoot:content-type*) "application/json")
                              (json:encode-json-to-string
                               (cond
                                 ((eql :error data)
                                  `((:state . "error")))
                                 (data
                                  `((:data . ,(markup:write-html data))
                                    (:state . "done")))
                                 (t
                                  `((:state . "processing"))))))))
    (make-thread (lambda ()
                   (ignore-and-log-errors ()
                     (handler-bind ((error (lambda (e)
                                             (declare (ignore e))
                                             (setf data :error))))
                       (let ((auth:*current-session* session)
                             (hunchentoot:*request* request)
                             (hunchentoot:*acceptor* acceptor))
                         (setf data (apply 'render-diff-report args)))))))
    <div class= "async-fetch spinner-border" role= "status" data-check-nibble=data-check-nibble />))


(defun warmup-comparison-images (run previous-run)
  (make-thread
   (lambda ()
     (warmup-comparison-images-sync run previous-run))
   :name "warmup-comparison-images"))

(auto-restart:with-auto-restart ()
  (defun warmup-comparison-images-sync (run previous-run)
    (let ((report (diff-report:make-diff-report run previous-run)))
      ;; warmup in the order that the report would be typically
      ;; viewed.
      (dolist (group (changes-groups report))
        (let ((changes (mapcar #'actual-item (diff-report:group-items group))))
          (loop for change in changes
                for i from 1
                for before = (diff-report:before change)
                for after = (diff-report:after change)
                for image-comparison-job = (make-instance 'image-comparison-job
                                                          :before-image before
                                                          :after-image after)
                do
                   (progn
                     (log:info "Warming up compare image ~d of ~d (~a)" i (length changes)
                               (screenshot-name after))
                     (restart-case
                         (prepare-image-comparison image-comparison-job :size nil
                                                                        :warmup t)
                       (ignore-this-image ()
                         nil)))))))))

(defun all-comparisons-page (report)
  <app-template>
    <a href= "javascript:window.history.back()">Back to Report</a>
    ,(paginated
      (lambda (change)
       (let* ((before (diff-report:before change))
              (after (diff-report:after change))
              (image-comparison-job (make-instance 'image-comparison-job
                                                    :before-image before
                                                    :after-image after))
              (comparison-image (util:copying (image-comparison-job)
                                    (nibble (:name :comparison)
                                      (prepare-image-comparison image-comparison-job :size :full-page)))))
         <div class= "image-comparison-wrapper" >
           <h3>,(screenshot-name before)</h3>
           <change-image-row-triple before-image=(image-public-url (screenshot-image before) :size :full-page)
                                    after-image=(image-public-url (screenshot-image after) :size :full-page)
                                    comp-image=comparison-image
                                    />
         </div>))
      :num 5
      :items (diff-report:diff-report-changes report))
  </app-template>)

(deftag progress-img (&key (alt "Image Difference") src class)
  "An IMG with a progress indicator for the image loading."

  <div class= (format nil  "progress-image-wrapper ~a" class) >
  <div class= "alert alert-danger images-identical" style= "display:none" >
    <p>
      <strong>The two images are identical.</strong> This is likely because the images still have their EXIF data, e.g. timestamps.
    </p>

    <p class= "mb-0" >
      You can pre-process images to remove timestamps. One way to do this is to use: `<:tt>exiftool -all= *.png</:tt>`.
    </p>

  </div>
    <div class= "loading">
      <div class="spinner-border" role="status">
        <!-- <span class="sr-only">Loading...</span> -->
      </div>
      Loading (this could take upto 30s in some cases)
    </div>

    <div>
      <div class= "alert alert-info">
        <strong>New interactive comparisons!</strong> Use your mouse to pan through the image. Use the <strong>mouse wheel</strong> to zoom into a location.
      </div>
      <div class= "canvas-container image-comparison-modal-image"
           data-src=src />
    </div>
  </div>)

(deftag zoom-to-change-button ()
  <button type="button" class="btn btn-secondary zoom-to-change">
    <div class="spinner-border" role="status" style="display:none; height: 1em; width: 1em" />
    Zoom to change
  </button>)

(defclass tab ()
  ((title :initarg :title
          :reader tab-title)
   (content :initarg :content
            :reader tab-content)))

(defun maybe-tabulate (tabs &key header &aux (id (format nil "a~a" (random 10000000))))
  (cond
    ((and (eql 1 (length tabs))
          (str:emptyp (tab-title (car tabs))))
     ;; don't show the tabulation
     <markup:merge-tag>
       <div class= "card-header">
         ,(progn header)
       </div>
       <div class= "card-body">
         ,(tab-content (car tabs))
       </div>
     </markup:merge-tag>)
    (t
     <markup:merge-tag>
       <div class= "card-header">
         ,(progn header)
         <ul class= "nav nav-tabs card-header-tabs" role= "tablist" >
           ,@ (loop for tab in tabs
                    for ctr from 0
                    collect
                    <li class= "nav-item" role= "presentation" >
                      <button class= (format nil "nav-link ~a" (if (= ctr 0) "active" ""))
                              data-bs-toggle= "tab"
                              data-bs-target= (format nil "#~a-~a" id ctr)
                              data-title= (tab-title tab)
                              role= "tab"
                              aria-controls= (format nil "~a-~a" id ctr)
                              aria-selector=(if (= ctr 0) "true" "false") >
                        ,(tab-title tab)
                      </button>
                    </li>)
         </ul>
       </div>

       <div class= "card-body">
         <div class= "tab-content">
           ,@(loop for tab in tabs
           for ctr from 0
           collect
           <div class= (format nil "tab-pane  ~a" (if (= ctr 0) "show active" ""))
                id= (format nil "~a-~a" id ctr)
                role= "tab-panel"
                aria-labelled-by= (tab-title tab) >
             ,(tab-content tab)
           </div>)
         </div>
         </div>

  </markup:merge-tag>)))

(defun make-overlay-image (before after)
  (let ((image-comparison-job (make-instance 'image-comparison-job
     :before-image before
     :after-image after)))
     (image-comparison-result
      (prepare-image-comparison-file image-comparison-job))))

(defun render-change-group (group run script-name &key search)
  <div class= "col-12">
  <div class= "card mb-3">
    ,(maybe-tabulate
      (loop for group-item in (diff-report:group-items group)
            for change = (actual-item group-item)
            for next-id = (random 1000000000000000)
            ;; TODO(T1273): there's a bug somewhere. diff-report:before here
            ;; becomes s, but later in change-image-row this is used
            ;; as the after image. See %find-changes in diff-report.lisp
            for s = (diff-report:before change)
            for x = (diff-report:after change)
            collect
    (make-instance
    'tab
    :title (diff-report:group-item-subtitle group-item)
    :content
    (let* ((s s)
           (x x)
           (toggle-id (format nil "toggle-id-~a" next-id)))

    <div class= "" >
      <div class= "screenshot-header" >
        <ul class= "screenshot-options-menu" >
          <li>
            <a href= "#" data-bs-toggle= "modal" data-bs-target= (format nil "#~a" toggle-id) >Compare</a>
          </li>
          <li>
            <a href= (hex:make-url "/channel/:channel/history" :channel (store-object-id (recorder-run-channel run))
                                                                                                                    :screenshot-name (screenshot-name x))>
              Full History
            </a>
          </li>
          <li>
            <a href= (nibble (:name :mask-editor) (mask-editor (recorder-run-channel run) s
               :redirect script-name
               :overlay (make-overlay-image x s)))
               target= "_blank" >
              Edit Masks
            </a>
          </li>

          ,(let ((id (format nil "a~a" (random 10000000000))))
             <li>
               <a href= "#" class= "dropdown-toggle" data-bs-toggle= "dropdown"
                  data-bs-target= id
                  aria-expanded= "false" >Download Original</a>
               <ul class= "dropdown-menu" >
                 <li>
                   <a class= "dropdown-item" href= (image-public-url (screenshot-image x) :originalp t)
                      >Download Previous image</a>
                 </li>
                 <li>
                   <a class= "dropdown-item" href= (image-public-url (screenshot-image s) :originalp t)
                      >Download Updated image</a>
                 </li>
               </ul>
             </li>)
        </ul>
      </div>
      <change-image-row before-image=(screenshot-image x)
                        after-image=(screenshot-image s)
                        before-dims= (ignore-errors (image-dimensions (screenshot-image x)))
                        after-dims= (ignore-errors (image-dimensions (screenshot-image s)))
                        />
      <comparison-modal before=x after=s toggle-id=toggle-id />
    </div>)))
      :header  <h4 class= "screenshot-title">,(highlight-search-term search (diff-report:group-title group))</h4>)
  </div>
  </div>)

(defun highlight-search-term (search title)
  (cond
    ((str:emptyp search)
     title)
    (t

     (let* ((start (search (str:downcase search) (str:downcase title)))
            (end (+ start (length search))))
       (markup:make-merge-tag
        (list
         <span class= "text-muted">,(subseq title 0 start)</span>
         <span class= "" >,(subseq title start end)</span>
         <span class= "text-muted">,(subseq title end)</span>))))))

(deftag comparison-modal (&key toggle-id before after)
  (let* ((modal-label (format nil "~a-modal-label" toggle-id))
         (image-comparison-job
           (make-instance 'image-comparison-job
                           :before-image before
                           :after-image after))
         (compare-nibble (nibble (:name :compare-link)
                           (prepare-image-comparison
                            image-comparison-job))))
    <div class= "modal fade image-comparison-modal" id= toggle-id tabindex= "-1" role= "dialog"
         aria-labelledby=modal-label
         aria-hidden= "true" >
      <div class="modal-dialog" role="document">
        <div class="modal-content">
          <div class="modal-header">
            <h5 class="modal-title" id=modal-label >Image Comparison</h5>
            <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close" />
          </div>
          <div class="modal-body">
            <progress-img
              src=compare-nibble
              alt= "Image difference" />
          </div>
          <div class="modal-footer">
            <button type="button" class="btn btn-secondary dropdown-toggle" data-bs-toggle="dropdown" aria-expanded="false" style= "margin-right: auto" >
              More
            </button>
            <ul class="dropdown-menu">
              <li>
                <a class= "dropdown-item view-item view-diff">
                  Show Diff
                </a>
              </li>
              <li>
                <a class= "dropdown-item view-item view-previous">
                  Show Previous Image
                </a>
              </li>
              <li>
                <a class= "dropdown-item view-item view-updated">
                  Show Updated Image
                </a>
              </li>
              <li>
                <hr class= "dropdown-divider" />
              </li>
              <li>
                <a class= "dropdown-item metrics-link" href= "#" >
                  Metrics
                </a>


              </li>

            </ul>

            <zoom-to-change-button />

            <button type="button" class="btn btn-primary" data-bs-dismiss="modal">Close</button>
          </div>
        </div>
      </div>
    </div>))


(defun metrics-page (image-comparison masks)
  (with-local-image (file (image-comparison-result image-comparison))
    (with-wand (wand :file file)
      (log:debug"random-zoom-to-on-result on ~a" file)
      (multiple-value-bind (pixels) (get-non-alpha-pixels wand :masks masks)
        (flet ((read-px-values (image)
                 (with-local-image (file image)
                   (with-wand (wand :file file)
                     (loop for _ from 0 to 3
                           for i from 0 below (car (array-dimensions pixels))
                           collect
                           (get-px-as-string wand
                                             (aref pixels i 0)
                                             (aref pixels i 1)))))))
         (let ((num-changed (car (array-dimensions pixels)))
               (width (magick-get-image-width wand))
               (height (magick-get-image-height wand))
               (before-values (read-px-values
                               (image-comparison-before image-comparison)))
               (after-values (read-px-values
                              (image-comparison-after image-comparison))))
           <app-template>
             <div class= "main-content">
               <div class= "card-page-container mx-auto">
                 <div class= "card mt-2">
                   <div class= "card-body">

                     <table class= "table" >
                       <thead>
                         <tr>
                           <td>
                             Metric
                           </td>
                           <td>
                             Value
                           </td>
                         </tr>
                       </thead>
                       <tbody>
                         <tr>
                           <td>
                             Dimensions
                           </td>
                           <td>
                             <span>
                               ,(progn width)x,(progn height)
                             </span>
                           </td>
                         </tr>

                         <tr>
                           <td>
                             # changed pixels
                           </td>
                           <td>
                             ,(if (>= num-changed 999)
                                  "> 999"
                                  num-changed)
                           </td>
                         </tr>

                         <tr>
                           <td>Fraction changed pixels</td>
                           <td>
                             ,(/ (* 1.0 num-changed) (* height width))
                           </td>
                         </tr>


                       </tbody>

                     </table>
                   </div>
                 </div>

                 <div class= "card mt-2">
                   <div class= "card-body">

                     <table class= "table">
                       <thead>
                         <tr>
                           <td>Position</td>
                           <td>Previous color</td>
                           <td>After color</td>
                         </tr>
                       </thead>
                       <tbody>
                       ,@ (loop for i below (car (array-dimensions pixels))
                                for before in before-values
                                for after in after-values
                                collect
                                <tr>
                                  <td> ,(aref pixels i 0),,(aref pixels i 1)</td>
                                  <td>,(progn before) </td>
                                  <td>,(progn after)</td>
                                </tr>)
                       </tbody>
                     </table>
                   </div>
                 </div>
               </div>
             </div>
           </app-template>))))))


(deftag compare-tab-a (body &key type default-type)
  <a class= (format nil "nav-link ~a" (when (string= type default-type) "active"))
     href= "#" data-type= type >
    ,@body
  </a>)

(deftag render-diff-report (children &key run to
                            more
                            acceptable
                            (re-run nil))
  (declare (ignore re-run))
  (let* ((report (diff-report:make-diff-report run to))
         (all-comparisons (nibble (:name :all-comparison)
                            (all-comparisons-page report))))
    (declare (ignorable all-comparisons))
    (let* ((changes-groups (diff-report:changes-groups report))
           (added-groups (diff-report:added-groups report))
           (deleted-groups (diff-report:deleted-groups report))
           (default-type
             (or
               (hunchentoot:parameter "type")
               (cond
                  (changes-groups "changes")
                  (added-groups "added")
                  (deleted-groups "deleted")
                  (t "changes")))))
      <markup:merge-tag>

      <div class= "mt-3 d-flex  flex-wrap justify-content-between compare-header" >

      <div class= "report-search-wrapper"  >
      <div class= "input-group">
      <span class= "input-group-text report-search" >
      <mdi name= "search" />
      </span>
      <input class= "form-control search d-inline-block" type= "text" autocomplete= "off"
      placeholder= "Search..."
      data-target= ".report-result" />
      <render-run-tags tags= (recorder-run-tags run) />
      </div>
      </div>

      <div class= "options" >
      <ul class= "nav nav-pills report-selector" data-target= ".report-result" >
      <li class= "nav-item">
        <compare-tab-a type= "changes" default-type=default-type >
          ,(length changes-groups) changes
        </compare-tab-a>
      </li>
      <li class= "nav-item">
        <compare-tab-a type= "added" default-type=default-type >
          ,(length added-groups) added
        </compare-tab-a>
      </li>
      <li class= "nav-item">
        <compare-tab-a type= "deleted" default-type=default-type >
          ,(length deleted-groups) deleted
        </compare-tab-a>
      </li>

      <markup:merge-tag>
      <li class= "nav-item" >
      <button type="button" class="btn btn-secondary dropdown-toggle" data-bs-toggle="dropdown" aria-expanded="false">
      More
      </button>
      <ul class="dropdown-menu dropdown-menu-end">
      ,@ (loop for (name . url) in more
               collect
               <li><a class="dropdown-item" href=url >,(progn name)</a></li>)
      <li><a role= "button" class= "dropdown-item" href= "#" data-bs-toggle="modal" data-bs-target= "#comparison-info-modal"><mdi name= "info"/> Info</a></li>
      ,(when-let ((url (?. run-build-url run)))
         <li><a role= "button" class= "dropdown-item" href=url target= "_blank">
             <mdi name= "build" /> View Build</a></li>)
      ,(when (and
              (?. adminp (current-user))
              *summarizer*)
         <li><a class= "dropdown-item" href= (nibble () (funcall *summarizer* report)) > [Admin] Summarize</a></li>)
      ,(progn
         #+screenshotbot-oss
         (progn
           <li>
           <a class= "dropdown-item" href=all-comparisons >All Pixel Comparisons (OSS only) </a>
           </li>))

      </ul>

      </li>

      ,(when (and acceptable (auth:can-viewer-edit (auth:viewer-context hunchentoot:*request*)
                                                   acceptable))
         <li class= "nav-item" >
         <render-acceptable acceptable=acceptable />
         </li>)
      </markup:merge-tag>
      </ul>



      </div>

      </div>

      ,@children

      <div class= "report-result mt-3"
           data-update= (nibble (:name :u-r-res) (report-result run
           changes-groups
           added-groups
           deleted-groups))
      data-args= (json:encode-json-to-string `((:type . ,default-type))) >
      ,(report-result run
                      changes-groups
                      added-groups
                      deleted-groups
                      :type default-type)
      </div>

      ,(info-modal run to)

      </markup:merge-tag>)))

(deftag link-to-run (&key run)
  (cond
    (run
     <span><a href= (hex:make-url "/runs/:id" :id (oid run))>run from ,(timeago :timestamp (created-at run))</a><render-run-tags tags= (recorder-run-tags run) /></span>)
    (t
     <span>empty run</span>)))

(defun info-modal (run to)
  <div class="modal" tabindex="-1" id= "comparison-info-modal" >
    <div class="modal-dialog">
      <div class="modal-content">
        <div class="modal-header">
          <h5 class="modal-title">,(channel-name (recorder-run-channel run)) </h5>
          <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
        </div>
        <div class="modal-body">
          <p>Comparing <link-to-run run=run /> to <link-to-run run=to />.</p>

          ,(when-let* ((repo (channel-repo (recorder-run-channel run)))
                       (this-hash (recorder-run-commit run))
                       (prev-hash (?. recorder-run-commit to)))
             (let ((review-link (review-link :run run)))
               <p class= "mt-2" >
                 This commit: <commit repo= repo hash=this-hash />
                 ,(when review-link
                    <span> on ,(progn review-link)</span>)
                 <br />
                 Previous commit: <commit repo= repo hash=prev-hash />
               </p>))
        </div>
        <div class="modal-footer">
          <button type="button" class="btn btn-secondary" data-bs-dismiss="modal">Close</button>
        </div>
      </div>
    </div>
  </div>)

(defun group-matches-p (group search)
  (or
   (str:emptyp search)
   (str:contains? search (diff-report:group-title group) :ignore-case t)))

(defun report-result (run changes-groups added-groups deleted-groups
                      &key
                      (type (hunchentoot:parameter "type")))

  (let ((search (hunchentoot:parameter "search")))
    (cond
      ((string-equal "added" type)
       (render-single-group-list added-groups :search search))
      ((string-equal "deleted" type)
       (render-single-group-list deleted-groups :search search))
      (t
       <div class= "">
           ,(paginated
             (lambda (group)
               (render-change-group group run (hunchentoot:script-name*)  :search search))
             :num 10
             :filter (lambda (group)
                       (group-matches-p group search))
             :items changes-groups
             :empty-view (no-screenshots))
       </div>))))

(defun render-single-group-list (groups &key search)
  (let* ((filter (lambda (group)
                   (group-matches-p group search)))
         (screenshots-viewer (make-instance 'screenshots-viewer
                                            :screenshots groups
                                            :filter filter
                                            :mapper (lambda (group)
                                                      (actual-item (car (diff-report:group-items group)))))))
    <div>
      ,(render-modal screenshots-viewer)

      ,(paginated
        (lambda (group i)
          <div class= "col-md-6">
            <div class= "card mb-3">
              ,(maybe-tabulate
                (loop for group-item in (diff-report:group-items group)
                      for screenshot = (actual-item group-item)
                      collect
                      (make-instance
                       'tab
                       :title (get-tab-title screenshot group)
                       :content
                       <div>
                         <div class= "screenshot-header" >
                           <ul class= "screenshot-options-menu" >
                             <li>
                               <a href= (image-public-url (screenshot-image screenshot) :originalp t)
                                  >Download Original</a>
                             </li>
                           </ul>
                         </div>

                         <a href= "#"
                            class= "screenshot-run-image"
                            data-image-number=i
                            data-target= (format nil "#~a" (modal-id screenshots-viewer)) >

                           <screenshot-box  screenshot=screenshot title= (diff-report:group-title group) />
                         </a>
                       </div>))
                :header
                <span>
                  <h4 class= "screenshot-title" >
                    ,(when (group-renamed-p group)
                       <span class= "badge bg-warning">Renamed</span>)
                      ,(highlight-search-term search (diff-report:group-title group))
                  </h4>

                </span>)
            </div>
          </div>)
        :num 12
        :filter filter
        :items groups
        :pass-index-p t
        :empty-view (no-screenshots))
    </div>))

(Deftag screenshot-box (&key screenshot title)
  (let ((dimensions (ignore-errors (image-dimensions (screenshot-image screenshot)))))
    <div class= "mt-1" >
      <picture-with-img
        class= "mt-2"
        image= (screenshot-image screenshot)
        dimensions=dimensions
        alt=title />
    </div>))

(defun no-screenshots ()
  <div class= "text-muted text-center">
    No changes match filters
  </div>)
