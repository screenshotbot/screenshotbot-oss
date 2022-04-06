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
        #:screenshotbot/model/image
        #:screenshotbot/model/view
        #:screenshotbot/model/report
        #:screenshotbot/model/channel
        #:screenshotbot/ignore-and-log-errors
        #:screenshotbot/model/recorder-run)
  (:shadow #:find)
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
  (:import-from #:screenshotbot/dashboard/paginated
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
  (:import-from #:screenshotbot/magick-lw
                #:get-non-alpha-pixels
                #:with-wand)
  (:import-from #:screenshotbot/diff-report
                #:get-tab-title)
  (:import-from #:bknr.datastore
                #:cascading-delete-object)
  (:import-from #:screenshotbot/taskie
                #:timeago)
  (:export
   #:diff-report
   #:render-acceptable
   #:make-diff-report
   #:diff-report-empty-p
   #:render-diff-report
   #:diff-report-changes
   #:warmup-comparison-images))
(in-package :screenshotbot/dashboard/compare)


(markup:enable-reader)

(defvar *lock* (bt:make-lock "image-comparison"))

(deftag render-acceptable (&key acceptable)
  (let ((accept (nibble (redirect)
                  (setf (acceptable-state acceptable) :accepted)
                  (hex:safe-redirect redirect)))
        (reject (nibble (redirect)
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
        <form action=accept method= "POST" class= "dropdown-item" >
          <button action= "submit" class= "btn btn-link acceptable accept-link" >
            <input type= "hidden" name= "redirect"
                   value= (hunchentoot:script-name*) />
            <i class= "mdi mdi-check-bold" />
            Accept
          </button>
        </form>

        <form action=reject method= "POST" class= "dropdown-item">
          <input type= "hidden" name= "redirect"
                 value= (hunchentoot:script-name* ) />
          <button action= "submit" class= "btn btn-link acceptable reject-link" >
            <i class= "mdi mdi-close" />
            Reject</button>
        </form>

      </div>
    </markup:merge-tag>))

(defun diff-report-empty-p (diff-report)
  (not
   (or (diff-report-added diff-report)
       (diff-report-deleted diff-report)
       (diff-report-changes diff-report))))

(defhandler (compare-page :uri "/runs/:id/compare/:to") (id to report)
  (when (string= "254" id)
    (hex:safe-redirect "/report/5fd16bcf4f4b3822fd000146"))
  (flet ((find-run (id)
           (let ((ret (find-by-oid id 'recorder-run)))
             (assert ret)
             ret)))
   (let* ((run (find-run id))
          (to (find-run to)))
     (can-view! run to)
     <app-template>
     ,(async-diff-report :run run :to to)
     </app-template>)))


(deftag change-image-row (&key before-image
                          after-image)
  <div class="change-image-row">
    <img class= "screenshot-image change-image change-image-left" src= before-image
         loading= "lazy" />

    <mdi name= "arrow_forward" />
    <img class= "screenshot-image change-image change-image-right" src= after-image
         loading= "lazy" />
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

(defclass image-comparison (store-object)
  ((before :initarg :before
           :reader image-comparison-before
           :index-type hash-index
           :index-reader %image-comparisons-for-before
           :relaxed-object-reference t)
   (after :initarg :after
          :reader image-comparison-after
          :relaxed-object-reference t)
   (masks :initarg :masks
          :reader image-comparison-masks)
   (identical-p :initform nil
                :accessor identical-p
                :initarg :identical-p
                :documentation "A result inducating that the images differ only in exif data")
   (result :initarg :result
           :accessor image-comparison-result))
  (:metaclass persistent-class))

(defmethod cascading-delete-p ((image image) (image-comparison image-comparison))
  "If the reference image is deleted, then delete the comparison."
  t)

(defmethod find-image-comparison-on-images ((before image)
                                            (after image)
                                            masks)
  "Finds an existing image comparison for before and after, if it
  doesn't exist calls creator with a temporary file. The creator
  should create the image in the file provided. The creator should
  returns true if the images are completely identical, or nil
  otherwise"
  (flet ((find ()
           (loop for comparison in (%image-comparisons-for-before before)
                 if (and (eql after (image-comparison-after comparison))
                         (equal masks (image-comparison-masks comparison)))
                   return comparison)))
    (or
     (bt:with-lock-held (*lock*)
       (find))
     (uiop:with-temporary-file (:pathname p :type "png" :prefix "comparison")
       (let ((identical-p (do-image-comparison
                            before
                            after
                            p
                            masks)))
         (let* ((image-blob (make-blob-from-file p 'image-blob :type :png))
                (image (make-image :blob image-blob
                                   :hash nil
                                   :verified-p nil ;; the hash is incorrect
                                   :content-type "image/png")))
           (bt:with-lock-held (*lock*)
             (or
              (find)
              (make-instance 'image-comparison
                              :before before
                              :after after
                              :masks masks
                              :identical-p identical-p
                              :result image)))))))))

(with-auto-restart ()
  (defmethod recreate-image-comparison ((self image-comparison))
    (log:info "recreating: ~a" (bknr.datastore:store-object-id self))
    (let* ((image (image-comparison-result self))
           (blob (image-blob image)))
      (check-type blob image-blob)
      (check-type image image)
      (restart-case
          (let ((pathname (bknr.datastore:blob-pathname blob)))
            (let ((before (image-comparison-before self))
                  (after (image-comparison-after self))
                  (masks (image-comparison-masks self)))
              (delete-object self)
              (find-image-comparison-on-images
               before after masks))
            (delete-object image)
            (delete-object blob)
            (log:info "Deleting ~a" pathname)
            (delete-file pathname))
        (ignore-this-comparison ()
          (values))))))

(defmethod recreate-all-image-comparisons ()
  (loop for image-comparison in (reverse
                                 (bknr.datastore:store-objects-with-class 'image-comparison))
        do
        (recreate-image-comparison image-comparison)))

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
          (:store-object-id . ,(bknr.datastore:store-object-id image-comparison))
          (:zoom-to . ,(nibble-url (nibble () (random-zoom-to-on-result
                                               image-comparison))))
          (:src . ,(image-public-url (image-comparison-result image-comparison) :size size))
          (:background . ,(image-public-url (screenshot-image (before-image self)) :size size))))))))


(defun random-zoom-to-on-result (image-comparison)
  (setf (hunchentoot:content-type*) "application/json")
  (with-local-image (file (image-comparison-result image-comparison))
    (with-wand (wand :file file)
      (let ((pxs (get-non-alpha-pixels wand)))
        (let ((i (random (car (array-dimensions pxs)))))
          (let ((dims (image-dimensions (image-comparison-result image-comparison))))
            (json:encode-json-to-string
             `((:y . ,(aref pxs i 1))
               (:x . ,(aref pxs i 0))
               (:width . ,(dimension-width dims))
               (:height . ,(dimension-height dims))))))))))

(defun do-image-comparison (before-image
                            after-image
                            p
                            masks)
  "Compares before-screenshot and after-screenshot, and saves the result image to P.

If the images are identical, we return t, else we return NIL."
  (with-local-image (before before-image)
    (with-local-image (after after-image)
      (let ((cmd (list
                  "compare"
                  "-metric" "RMSE"
                  (namestring before)
                  (namestring after)
                  "-highlight-color" "red"
                  "-lowlight-color" "none"
                  "-compose" "src"
                  (namestring p))))
        (multiple-value-bind (out err ret)
            (run-magick cmd
                        :ignore-error-status t
                        :output 'string
                        :error-output 'string)

          (unless (member ret '(0 1))
            (error "Got surprising error output from imagemagic compare: ~S~%args:~%~S~%stderr:~%~a~%stdout:~%~a" ret cmd err out))
          (draw-masks-in-place p masks :color "rgba(255, 255, 0, 0.8)")
          (log:info "Got return code: ~a with output `~a`, `~a`" ret out err)
          (when (= ret 0)
            (equal "0 (0)" (str:trim err))

            ;; Slow version if we ever find that imagemagick's output
            ;; is unreliable:
            #+nil
            (block check
              (map-unequal-pixels
               before-screenshot after-screenshot
               (lambda (x y)
                 (declare (ignore x y))
                 (return-from check nil)))
              t)))))))


(defun async-diff-report (&rest args &key &allow-other-keys)
  (let* ((data nil)
         (session auth:*current-session*)
         (request hunchentoot:*request*)
         (acceptor hunchentoot:*acceptor*)
         (data-check-nibble (nibble ()
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
          (let ((changes (diff-report-changes report)))
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
                       (prepare-image-comparison image-comparison-job :size nil
                                                                      :warmup t)))))
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
                                    (nibble ()
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
                              role=tab
                              aria-controls= (format nil "~a-~a" id ctr)
                              aria-selector (if (= ctr 0) "true" "false") >
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
            <a href= (nibble () (mask-editor (recorder-run-channel run) s
               :redirect script-name))
               >
              Edit Masks
            </a>
          </li>
        </ul>
      </div>
      <change-image-row before-image=(image-public-url (screenshot-image x) :size :full-page)
                        after-image=(image-public-url (screenshot-image s) :size :full-page)
                        />
      <comparison-modal before=x after=s toggle-id=toggle-id />
    </div>)))
      :header  <h4>,(highlight-search-term search (group-title group))</h4>)
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
         (compare-nibble (nibble ()
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



(deftag render-diff-report (&key run to
                            (lang-filter (make-instance 'row-filter :value t))
                            (device-filter (make-instance 'row-filter :value t))
                            more
                            acceptable
                            (re-run nil))
  (flet ((filteredp (x) (and (run-row-filter lang-filter x)
                             (run-row-filter device-filter x))))
   (let* ((report (make-diff-report run to))
          (script-name (hunchentoot:script-name*))
          (all-comparisons (nibble ()
                             (all-comparisons-page report))))
     (declare (ignorable all-comparisons))
     (let* ((added (diff-report-added report))
            (added (loop for s in added
                         if (filteredp s)
                           collect s))
            (deleted (diff-report-deleted report))
            (deleted (loop for s in deleted
                           if (filteredp s)
                             collect s))
            (changes (diff-report-changes report))
            (changes (loop for change in changes
                           if (or (filteredp (before change))
                                  (filteredp (after change)))
                             collect change))
            (changes-groups (changes-groups report))
            (added-groups (added-groups report))
            (deleted-groups (deleted-groups report)))
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
               <a class= "nav-link active" href= "#" data-type= "changes" >,(length changes-groups) changes</a>
             </li>
             <li class= "nav-item">
               <a class= "nav-link" href= "#" data-type= "added" >,(length added-groups) added</a>
             </li>
             <li class= "nav-item">
               <a class= "nav-link" href= "#" data-type= "deleted" >,(length deleted-groups) deleted</a>
             </li>

           ,(when more

             <markup:merge-tag>
               <li class= "nav-item" >
                 <button type="button" class="btn btn-secondary dropdown-toggle" data-bs-toggle="dropdown" aria-expanded="false">
                   More
                 </button>
                 <ul class="dropdown-menu dropdown-menu-end">
                   ,@ (loop for (name . url) in more
                            collect
                            <li><a class="dropdown-item" href=url >,(progn name)</a></li>)
                            <li><a class= "dropdown-item" href= "#" data-bs-toggle="modal" data-bs-target= "#comparison-info-modal">Info</a></li>
                            ,(progn
                               #+screenshotbot-oss
                               (progn
                                 <li>
                                   <a class= "dropdown-item" href=all-comparisons >All Pixel Comparisons (OSS only) </a>
                                 </li>))

                 </ul>

               </li>

               ,(when acceptable
                  <li class= "nav-item" >
                    <render-acceptable acceptable=acceptable />
                  </li>)
             </markup:merge-tag>)
           </ul>



         </div>

       </div>

       <div class= "report-result mt-3" data-update= (nibble () (report-result run
            changes-groups
            added-groups
            deleted-groups))
            data-args= "{}" >
         ,(report-result run
                         changes-groups
                         added-groups
                         deleted-groups)
       </div>

       ,(info-modal run to)

       </markup:merge-tag>))))

(deftag link-to-run (&key run)
  <a href= (hex:make-url "/runs/:id" :id (oid run))>run from ,(timeago :timestamp (created-at run))</a>)

(defun info-modal (run to)
  <div class="modal" tabindex="-1" id= "comparison-info-modal" >
    <div class="modal-dialog">
      <div class="modal-content">
        <div class="modal-header">
          <h5 class="modal-title">More about this report</h5>
          <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
        </div>
        <div class="modal-body">
          <p>Comparing <link-to-run run=run /> to <link-to-run run=to />.</p>

          ,(when-let* ((repo (channel-repo (recorder-run-channel run)))
                       (this-hash (recorder-run-commit run))
                       (prev-hash (recorder-run-commit to)))
             <p class= "mt-2" >
               This commit: <commit repo= repo hash=this-hash /> <br />
               Previous commit: <commit repo= repo hash=prev-hash />
             </p>)
        </div>
        <div class="modal-footer">
          <button type="button" class="btn btn-secondary" data-bs-dismiss="modal">Close</button>
        </div>
      </div>
    </div>
  </div>)

(defun filter-groups (groups search)
  (loop for group in groups
        if (or
            (str:emptyp search)
            (str:contains? search (group-title group) :ignore-case t))
          collect group))

(defun report-result (run changes-groups added-groups deleted-groups)

  (let ((type (hunchentoot:parameter "type"))
        (search (hunchentoot:parameter "search")))
    (cond
      ((string-equal "added" type)
       (render-single-group-list (filter-groups added-groups search) :search search))
      ((string-equal "deleted" type)

       (render-single-group-list (filter-groups deleted-groups search) :search search))
      (t
       <div class= "">
           ,(let ((filtered-groups (filter-groups changes-groups search)))
              (cond
                (filtered-groups
                 (paginated
                  (lambda (group)
                    (render-change-group group run (hunchentoot:script-name*)  :search search))
                  :num 10
                  :items filtered-groups))
                (t
                 (no-screenshots))))
       </div>))))

(defun render-single-group-list (groups &key search)
  (cond
    (groups
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
              :header <h4>,(highlight-search-term search (group-title group)) </h4>)
          </div>
        </div>)
      :num 5
      :items groups))
    (t
     (no-screenshots))))

(Deftag screenshot-box (&key screenshot title)
  <div class= "mt-4" >
    <div class= "screenshot-header">
      <h4>,(progn title) </h4>
    </div>

    <img class= "mt-2 screenshot-image change-image" src= (image-public-url (screenshot-image screenshot) :size :full-page) loading= "lazy" />
  </div>)

(defun no-screenshots ()
  <div class= "text-muted text-center">
    No changes match filters
  </div>)
