;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/compare
  (:use #:cl
        #:alexandria
        #:nibble
        #:screenshotbot/template
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
                #:defhandler)
  (:import-from #:screenshotbot/report-api
                #:render-diff-report)
  (:import-from #:screenshotbot/dashboard/run-page
                #:run-row-filter
                #:page-nav-dropdown
                #:row-filter
                #:mask-editor
                #:filter-selector
                #:commit)
  (:import-from #:screenshotbot/model/image
                #:image-blob
                #:rect-as-list
                #:draw-masks-in-place
                #:random-unequal-pixel
                #:find-unequal-pixels)
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
                #:current-company)
  (:export
   #:diff-report
   #:render-acceptable
   #:diff-report-title
   #:make-diff-report
   #:diff-report-empty-p
   #:render-diff-report
   #:diff-report-changes)
  ;; forward decls
  (:export #:filter-selector))
(in-package :screenshotbot/compare)


(markup:enable-reader)

(defvar *lock* (bt:make-lock "image-comparison"))

(defclass change ()
  ((before :initarg :before
           :reader before)
   (after :initarg :after
          :reader after)
   (masks :initarg :masks
          :reader change-masks)))

(defclass diff-report ()
  ((added :initarg :added
          :reader diff-report-added
          :initform nil)
   (deleted :initarg :deleted
            :reader diff-report-deleted
            :initform nil)
   (changes :initarg :changes
            :initform nil
            :accessor diff-report-changes
            :documentation "List of all CHANGEs")))

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
    <div class="dropdown">
  <button class= (format nil"btn btn-sm btn-secondary dropdown-toggle ~a" btn-class) type="button" id="dropdownMenuButton" data-bs-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
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
  </div>))

(defun diff-report-title (diff-report)
  (let ((added (diff-report-added diff-report))
        (deleted (diff-report-deleted diff-report))
        (changes (diff-report-changes diff-report)))
    (str:join ", "
              (remove-if 'null
               (list
                (when changes
                  (format nil "~d changes" (length changes)))
                (when deleted
                  (format nil "~d deleted" (length deleted)))
                (when added
                  (format nil "~d added" (length added))))))))

(defun diff-report-empty-p (diff-report)
  (not
   (or (diff-report-added diff-report)
       (diff-report-deleted diff-report)
       (diff-report-changes diff-report))))

(defun make-diff-report (run to)
  (restart-case
      (flet ((screenshot-name= (x y)
               (string= (screenshot-name x) (screenshot-name y))))
        (let ((names (recorder-run-screenshots run))
              (to-names (recorder-run-screenshots to)))
          (make-instance
           'diff-report
           :added (set-difference names to-names :test #'screenshot-name=)
           :deleted (set-difference to-names names :test #'screenshot-name=)
           :changes (loop for s1 in names appending
                                          (loop for x in to-names
                                                if (and
                                                    (string= (screenshot-name s1) (Screenshot-name x))
                                                    (not (image= (screenshot-image s1)
                                                                 (Screenshot-image x)
                                                                 ;; always use the new mask
                                                                 (screenshot-masks s1))))
                                                  collect
                                                  (make-instance 'change
                                                                 :before s1
                                                                 :masks (screenshot-masks s1)
                                                                 :after x))))))
    (retry-make-diff-report ()
      (make-diff-report run to))))

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
     ,(async-diff-report :run run :to to :disable-filters t)
     </app-template>)))


(deftag change-image-row (&key before-image
                          after-image)
  <div class="change-image-row">
    <img class= "screenshot-image change-image change-image-left" src= before-image />

    <mdi name= "arrow_forward" />
    <img class= "screenshot-image change-image change-image-right" src= after-image />
  </div>)

(deftag change-image-row-triple (&key before-image
                          after-image
                          comp-image)
  <div class="change-image-row">
    <img class= "screenshot-image change-image change-image-left" src= before-image width= 300 />
    <img class= "screenshot-image change-image change-image-right" src= after-image width= 300 />
    <:img data-src= comp-image
      class= "bg-primary image-comparison-modal-image" alt= "Image Difference" width= 300 />
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
   (output :initform nil
           :accessor output-image
           :documentation "An image object representing the final image")))

(defclass image-comparison (store-object)
  ((before :initarg :before
           :reader image-comparison-before
           :index-type hash-index
           :index-reader %image-comparisons-for-before)
   (after :initarg :after
          :reader image-comparison-after)
   (masks :initarg :masks
          :reader image-comparison-masks)
   (result :initarg :result
           :accessor image-comparison-result))
  (:metaclass persistent-class))


(defun find-image-comparison (before after masks creator)
  "Finds an existing image comparison for before and after, if it
  doesn't exist calls creator with a temporary file. The creator
  should create the image in the file provided."
  (check-type before image)
  (check-type after image)
  (flet ((find ()
           (loop for comparison in (%image-comparisons-for-before before)
                 if (and (eql after (image-comparison-after comparison))
                         (equal masks (image-comparison-masks comparison)))
                   return comparison)))
    (or
     (bt:with-lock-held (*lock*)
       (find))
     (uiop:with-temporary-file (:pathname p :type "png" :prefix "comparison")
       (funcall creator p)
       (let* ((image-blob (make-blob-from-file p 'image-blob :type :png))
              (image (make-instance 'image
                                     :blob image-blob
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
                            :result image))))))))

(defmethod prepare-image-comparison-file ((self image-comparison-job))
  (bt:with-lock-held ((lock self))
    (cond
      ((donep self)
       (output-image self))
      (t
       ;; second level of caching, we're going to look through the
       ;; datastore to see if there are any previous images
       (setf (output-image self)
             (image-comparison-result
              (find-image-comparison
               (screenshot-image (before-image self))
               (screenshot-image (after-image self))
               (screenshot-masks (after-image self))
               (lambda (output-file)
                 (do-image-comparison
                     (before-image self)
                   (after-image self)
                   output-file)))))))))

(defmethod prepare-image-comparison ((self image-comparison-job)
                                     &key
                                       ;; I can't use :full-page here because the JS isn't
                                       ;; designed to handle that yet.
                                       (size nil))
  (let ((image (prepare-image-comparison-file self)))
    (hex:safe-redirect (image-public-url image :size size))))

(defun do-image-comparison (before-screenshot
                            after-screenshot
                            p)
  (with-local-image (before before-screenshot)
    (with-local-image (after after-screenshot)
      (let ((cmd (list
                  "compare" (namestring before)
                  "-limit" "memory" "3MB"
                  "-limit" "disk" "500MB"
                  (namestring after)
                  (namestring p))))
        (multiple-value-bind (out err ret)
            (run-magick cmd
                        :ignore-error-status t
                        :output 'string
                        :error-output 'string)

          (unless (member ret '(0 1))
            (error "Got surprising error output from imagemagic compare: ~S~%args:~%~S~%stderr:~%~a~%stdout:~%~a" ret cmd err out))))
      (setf (hunchentoot:header-out :content-type)
            "image/png")
      (draw-masks-in-place p (screenshot-masks after-screenshot) :color "rgba(255, 255, 0, 0.8)")
      p)))

(defun is-image-similar (before-screenshot
                            after-screenshot)
  (with-local-image (before before-screenshot)
    (with-local-image (after after-screenshot)
      (let ((cmd (list
                  "compare" (namestring before)
                  "-limit" "memory" "3MB"
                  "-limit" "disk" "500MB"
                  (namestring after)
                  "mock.png")))
        (multiple-value-bind (out err ret)
            (run-magick cmd
                        :ignore-error-status t
                        :output 'string
                        :error-output 'string)

          (unless (member ret '(0 1))
            (error "Got surprising error output from imagemagic compare: ~S~%args:~%~S~%stderr:~%~a~%stdout:~%~a" ret cmd err out))
            ret)))))

(defun async-diff-report (&rest args &key &allow-other-keys)
  (let* ((data nil)
         (session auth:*current-session*)
         (request hunchentoot:*request*)
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
    (bt:make-thread (lambda ()
                      (ignore-and-log-errors ()
                        (handler-bind ((error (lambda (e)
                                                (declare (ignore e))
                                                (setf data :error))))
                          (let ((auth:*current-session* session)
                                (hunchentoot:*request* request))
                            (setf data (apply 'render-diff-report args)))))))
    <div class= "async-fetch spinner-border" role= "status" data-check-nibble=data-check-nibble />))


(defun random-zoom-to (left right)
  (setf (hunchentoot:header-out :content-type)  "application/json")
  (json:encode-json-to-string

   (let ((px (random-unequal-pixel
              (screenshot-image left)
              (screenshot-image right)
              :masks (screenshot-masks right))))
     `((:y . ,(car px))
       (:x . ,(cdr px))
       ;; for debugging:
       (:masks
        ,(mapcar 'rect-as-list
                 (screenshot-masks right)))))))

(defun all-comparisons-page (report)
  <app-template>
    <a href= "javascript:window.history.back()">Back to Report</a>
    ,(paginated
        (let ((changes (diff-report-changes report)))
          (loop for change in changes
                for before = (before change)
                for after = (after change)

                for image-campare-ret = (is-image-similar before after)
                collect
                <div class= "image-comparison-wrapper" >
                <h3>,(if (= image-campare-ret 1) (screenshot-name before) (concatenate 'string (screenshot-name before) " are the same"))</h3>
                ,(when (= image-campare-ret 1)
                  (let* ((image-comparison-job (make-instance 'image-comparison-job
                                                           :before-image before
                                                           :after-image after))
                        (comparison-image (util:copying (image-comparison-job)
                                         (nibble ()
                                           (prepare-image-comparison image-comparison-job :size :full-page)))))
                        <change-image-row-triple before-image=(image-public-url (screenshot-image before) :size :full-page)
                                         after-image=(image-public-url (screenshot-image after) :size :full-page)
                                         comp-image=comparison-image
                                         />))
                </div>))
        5)
  </app-template>)

(deftag progress-img (&key alt src zoom-to class)
  "An <img> with a progress indicator for the image loading."

  <div class= (format nil  "progress-image-wrapper ~a" class) >
    <div class= "loading">
      <div class="spinner-border" role="status">
        <!-- <span class="sr-only">Loading...</span> -->
      </div>
      Loading (this might take a while for large images)
    </div>
    <:img data-src= src
          data-zoom-to=zoom-to
          class= "bg-primary image-comparison-modal-image" alt= "Image Difference" />
  </div>)

(deftag zoom-to-change-button ()
  <button type="button" class="btn btn-secondary zoom-to-change">
    <div class="spinner-border" role="status" style="display:none; height: 1em; width: 1em" />
    Zoom to change
  </button>)

(deftag render-diff-report (&key run to
                            (lang-filter (make-instance 'row-filter :value t))
                            (device-filter (make-instance 'row-filter :value t))
                            disable-filters
                            acceptable
                            (re-run nil))
  (flet ((filteredp (x) (and (run-row-filter lang-filter x)
                             (run-row-filter device-filter x))))
   (let* ((report (make-diff-report run to))
          (next-id 0)
          (script-name (hunchentoot:script-name*))
          (all-comparisons (nibble ()
                            (all-comparisons-page report))))
     (let ((added (diff-report-added report))
           (deleted (diff-report-deleted report))
           (changes (diff-report-changes report)))
       <markup:merge-tag>
       <div class= "page-title-box">
       ,(when acceptable
          <render-acceptable acceptable=acceptable />)
           ,(unless disable-filters
              (let ((all-runs (append added deleted (mapcar 'before changes) (mapcar 'after changes))))
                <markup:merge-tag>
                  <filter-selector default-title= "All Languages"
                                   prefix= "Language"
                                   row-filter=lang-filter
                                   filter-renderer= (lambda (x) (funcall re-run :lang-filter x))
                                   data= all-runs
                                   />
                  <filter-selector default-title= "All Devices"
                                   prefix= "Device"
                                   row-filter=device-filter
                                   filter-renderer= (lambda (x) (funcall re-run :device-filter x))
                                   data= all-runs
                                   />
                </markup:merge-tag>))

           ,(progn
              #+screenshotbot-oss
              (progn
                <page-nav-dropdown title= "Views" >
                  <a href=all-comparisons >All Pixel Comparisons</a>
               </page-nav-dropdown>))

         </div>
         <p class= "mt-2" >
           This commit: <commit repo= (Channel-repo (recorder-run-channel run)) hash=(recorder-run-commit run) /> <br />
           Previous commit: <commit repo= (channel-repo (recorder-run-channel run)) hash=(recorder-run-commit to) />
         </p>


       <div class= "card mt-3">
         <div class= "card-body">
           <p>
             <h1>,(length changes) changes</h1>
       ,(paginated (loop for change in changes
                 for s = (before change)
                 for x = (after change)
                 if (or (filteredp s) (filteredp x))
                   collect
                   (let* ((s s)
                          (x X)
                          (image-comparison-job
                            (make-instance 'image-comparison-job
                                            :before-image x
                                            :after-image s))
                          (compare-nibble (nibble ()
                                            (prepare-image-comparison
                                             image-comparison-job)))
                          (zoom-to-nibble (nibble ()
                                            (random-zoom-to x s)))
                          (toggle-id (format nil "toggle-id-~a" (incf next-id)))
                          (modal-label (format nil "~a-modal-label" toggle-id)))

                     <div class= "mt-4" >
                       <ul class= "compare-image-header" >
                         <li>
                           <h4 class= "d-inline-block" >
                             ,(screenshot-name s)
                           </h4>
                         </li>
                         <li>
                           <a href= "#" data-bs-toggle= "modal" data-bs-target= (format nil "#~a" toggle-id) >Compare</a>
                         </li>
                         <li>
                           <a href= (nibble () (mask-editor (recorder-run-channel run) s
                              :redirect script-name))
                              >Edit Masks</a>
                         </li>
                       </ul>
                       <change-image-row before-image=(image-public-url (screenshot-image x) :size :full-page)
                                         after-image=(image-public-url (screenshot-image s) :size :full-page)
                                         />

                       <div class= "modal fade image-comparison-modal" id= toggle-id tabindex= "-1" role= "dialog"
                            aria-labelledby=modal-label
                            aria-hidden= "true" >
                         <div class="modal-dialog" role="document">
                           <div class="modal-content">
                             <div class="modal-header">
                               <h5 class="modal-title" id=modal-label >Image Comparison</h5>
                               <button type="button" class="close" data-bs-dismiss="modal" aria-label="Close">
                                 <span aria-hidden="true">
                                   &times
                                 </span>
                               </button>
                             </div>
                             <div class="modal-body">
                               <progress-img
                                 src=compare-nibble
                                 zoom-to=zoom-to-nibble
                                 alt= "Image difference" />
                             </div>
                             <div class="modal-footer">
                               <zoom-to-change-button />

                               <button type="button" class="btn btn-primary" data-bs-dismiss="modal">Close</button>
                             </div>
                           </div>
                         </div>
                       </div>
                     </div>))
                   10)
           </p>

         </div>
       </div>

       <div class= "row mt-4">
         <div class= "col-md-6">

           <div class= "card">
             <div class= "card-body">
               <h1>Deleted</h1>
               <p>
                 ,(paginated (loop for c in deleted
                                   if (filteredp c)
                                     collect
                                   <screenshot-box screenshot=c />)
                               10)
               </p>

             </div>
           </div>
         </div>
       <div class= "col-md-6">
         <div class= "card">
           <div class= "card-body">
             <h1>Added</h1>
             ,(paginated
               (loop for c in added
                     if (filteredp c)
                       collect
                     <screenshot-box screenshot=c />)
               10)

           </div>
         </div>
       </div>
       </div>
       </markup:merge-tag>))))

(Deftag screenshot-box (&key screenshot)
  <div class= "mt-4" >
    <h4>,(screenshot-name screenshot)</h4>
    <img class= "change-image" src= (image-public-url (screenshot-image screenshot) :size :full-page) />
  </div>)
