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
        #:screenshotbot/diff-report
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
                #:rect-as-list
                #:draw-masks-in-place)
  (:import-from #:screenshotbot/magick
                #:run-magick)
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
                #:get-non-alpha-pixels
                #:with-wand)
  (:import-from #:screenshotbot/diff-report
                #:actual-item
                #:changes-groups
                #:get-tab-title)
  (:import-from #:bknr.datastore
                #:cascading-delete-object)
  (:import-from #:screenshotbot/taskie
                #:timeago)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:screenshotbot/model/transient-object
                #:with-transient-copy)
  (:import-from #:screenshotbot/model/image-comparison
                #:find-image-comparison-on-images)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:screenshotbot/model/view
                #:can-edit
                #:can-edit!)
  (:import-from #:screenshotbot/dashboard/review-link
                #:review-link)
  (:export
   #:diff-report
   #:render-acceptable
   #:make-diff-report
   #:diff-report-empty-p
   #:render-diff-report
   #:diff-report-changes
   #:warmup-comparison-images))
(in-package :screenshotbot/dashboard/compare)

;; fake symbols for bknr migration
(progn
 '(result
   before
   after
   identical-p
   result))

(markup:enable-reader)

(deftag render-acceptable (&key acceptable)
  (let ((accept (nibble (redirect :name :accept)
                  (setf (acceptable-state acceptable) :accepted)
                  (hex:safe-redirect redirect)))
        (reject (nibble (redirect :name :reject)
                  (setf (acceptable-state acceptable) :rejected)
                  (hex:safe-redirect redirect)))
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
    <markup:merge-tag>
      <button class= (format nil"btn  btn-secondary dropdown-toggle ~a" btn-class) type="button" id="dropdownMenuButton" data-bs-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
        ,(progn btn-text)
      </button>
      <div class="dropdown-menu" aria-labelledby="dropdownMenuButton" style= "z-index: 99999999; position: static" >
        <form action=accept method= "POST" >
          <button action= "submit" class= "btn btn-link acceptable accept-link dropdown-item" >
            <input type= "hidden" name= "redirect"
                   value= (hunchentoot:script-name*) />
            <mdi name= "check" />
            Accept
          </button>
        </form>

        <form action=reject method= "POST">
          <input type= "hidden" name= "redirect"
                 value= (hunchentoot:script-name* ) />
          <button action= "submit" class= "btn btn-link acceptable reject-link dropdown-item" >
            <mdi name= "close" />
            Reject</button>
        </form>

      </div>
    </markup:merge-tag>))

(defun diff-report-empty-p (diff-report)
  (not
   (or (diff-report-added diff-report)
       (diff-report-deleted diff-report)
       (diff-report-changes diff-report))))

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


(defmethod find-image-comparison ((before-screenshot screenshot)
                                  (after-screenshot screenshot))
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
     after
     (screenshot-masks after-screenshot))))

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
       (json:encode-json-to-string
        `((:identical . ,(identical-p image-comparison))
          ;; for debugging: e.g. if we need to delete the comparison
          (:store-object-id
           .
           ,(when (typep image-comparison 'store-object)
              (bknr.datastore:store-object-id image-comparison)))
          (:zoom-to . ,(nibble-url (nibble (:name :zoom) (random-zoom-to-on-result
                                                          image-comparison))))
          (:src . ,(image-public-url (image-comparison-result image-comparison) :size size))
          (:background . ,(image-public-url (screenshot-image (before-image self)) :size size))))))))


(defun random-zoom-to-on-result (image-comparison)
  (setf (hunchentoot:content-type*) "application/json")
  (with-local-image (file (image-comparison-result image-comparison))
    (with-wand (wand :file file)
      (let ((pxs (get-non-alpha-pixels wand
                                       :masks (image-comparison-masks image-comparison))))
        (let ((i (random (car (array-dimensions pxs)))))
          (let ((dims (image-dimensions (image-comparison-result image-comparison))))
            (json:encode-json-to-string
             `((:y . ,(aref pxs i 1))
               (:x . ,(aref pxs i 0))
               (:width . ,(dimension-width dims))
               (:height . ,(dimension-height dims))))))))))


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
     (restart-case
         (let ((report (make-diff-report run previous-run)))
           ;; warmup in the order that the report would be typically
           ;; viewed.
           (dolist (group (changes-groups report))
            (let ((changes (mapcar #'actual-item (group-items group))))
              (loop for change in changes
                    for i from 1
                    for before = (before change)
                    for after = (after change)
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
                             nil)))))))
       (retry-warmup-thread ()
         (warmup-comparison-images run previous-run))))
   :name "warmup-comparison-images"))

(defun all-comparisons-page (report)
  <app-template>
    <a href= "javascript:window.history.back()">Back to Report</a>
    ,(paginated
      (lambda (change)
       (let* ((before (before change))
              (after (after change))
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
      :items (diff-report-changes report))
  </app-template>)

(deftag progress-img (&key (alt "Image Difference") src class)
  "An <img> with a progress indicator for the image loading."

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

  ,(cond
     ((hunchentoot:parameter "old-compare")
      <:img data-src= src
            class= "bg-primary image-comparison-modal-image" alt=alt />)
     (t
      <div>
        <div class= "alert alert-info">
          <strong>New interactive comparisons!</strong> Use your mouse to pan through the image. Use the <strong>mouse wheel</strong> to zoom into a location.
        </div>
        <:canvas data-src=src
                 class= "image-comparison-modal-image" />
      </div>))
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

(defun render-change-group (group run script-name &key search)
  <div class= "col-12">
  <div class= "card mb-3">
    ,(maybe-tabulate
      (loop for group-item in (group-items group)
            for change = (actual-item group-item)
            for next-id = (random 1000000000000000)
            for s = (before change)
            for x = (after change)
            collect
    (make-instance
    'tab
    :title (group-item-subtitle group-item)
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
               :redirect script-name))
               >
              Edit Masks
            </a>
          </li>
        </ul>
      </div>
      <change-image-row before-image=(screenshot-image x)
                        after-image=(screenshot-image s)
                        before-dims= (ignore-errors (image-dimensions (screenshot-image x)))
                        after-dims= (ignore-errors (image-dimensions (screenshot-image s)))
                        />
      <comparison-modal before=x after=s toggle-id=toggle-id />
    </div>)))
      :header  <h4 class= "screenshot-title">,(highlight-search-term search (group-title group))</h4>)
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
            <zoom-to-change-button />

            <button type="button" class="btn btn-primary" data-bs-dismiss="modal">Close</button>
          </div>
        </div>
      </div>
    </div>))


(deftag compare-tab-a (body &key type default-type)
  <a class= (format nil "nav-link ~a" (when (string= type default-type) "active"))
     href= "#" data-type= type >
    ,@body
  </a>)

(deftag render-diff-report (&key run to
                            more
                            acceptable
                            (re-run nil))
  (declare (ignore re-run))
  (let* ((report (make-diff-report run to))
         (all-comparisons (nibble (:name :all-comparison)
                            (all-comparisons-page report))))
    (declare (ignorable all-comparisons))
    (let* ((changes-groups (changes-groups report))
           (added-groups (added-groups report))
           (deleted-groups (deleted-groups report))
           (default-type
             (cond
               (changes-groups "changes")
               (added-groups "added")
               (deleted-groups "deleted")
               (t "changes"))))
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
      ,(progn
         #+screenshotbot-oss
         (progn
           <li>
           <a class= "dropdown-item" href=all-comparisons >All Pixel Comparisons (OSS only) </a>
           </li>))

      </ul>

      </li>

      ,(when (and (can-edit run (current-user)) acceptable)
         <li class= "nav-item" >
         <render-acceptable acceptable=acceptable />
         </li>)
      </markup:merge-tag>
      </ul>



      </div>

      </div>

      <div class= "report-result mt-3"
           data-update= (nibble (:name :u-r-res) (report-result run
           changes-groups
           added-groups
           deleted-groups))
      data-args= "{}" >
      ,(report-result run
                      changes-groups
                      added-groups
                      deleted-groups
                      :type default-type)
      </div>

      ,(info-modal run to)

      </markup:merge-tag>)))

(deftag link-to-run (&key run)
  <a href= (hex:make-url "/runs/:id" :id (oid run))>run from ,(timeago :timestamp (created-at run))</a>)

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
                       (prev-hash (recorder-run-commit to)))
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
   (str:contains? search (group-title group) :ignore-case t)))

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
  (paginated
   (lambda (group)
     <div class= "col-md-6">
     <div class= "card mb-3">
     ,(maybe-tabulate
       (loop for group-item in (group-items group)
             for screenshot = (actual-item group-item)
             collect
             (make-instance
              'tab
              :title (get-tab-title screenshot)
              :content
              <screenshot-box  screenshot=screenshot title= (group-title group) /> ))
       :header <h4 class= "screenshot-title" >,(highlight-search-term search (group-title group)) </h4>)
     </div>
     </div>)
   :num 12
   :filter (lambda (group)
             (group-matches-p group search))

   :items groups
   :empty-view (no-screenshots)))

(Deftag screenshot-box (&key screenshot title)
  (let ((dimensions (ignore-errors (image-dimensions (screenshot-image screenshot)))))
    <div class= "mt-4" >
      <div class= "screenshot-header">
        <h4>,(progn title) </h4>
      </div>

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
