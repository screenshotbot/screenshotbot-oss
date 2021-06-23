;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/compare
    (:use #:cl
          #:alexandria
          #:nibble
          #:./template
          #:./model/screenshot
          #:./model/image
          #:./model/view
          #:./model/report
          #:./model/channel
          #:./ignore-and-log-errors
          #:./model/recorder-run)
  (:import-from #:util
                #:find-by-oid
                #:oid)
  (:import-from #:markup
                #:deftag)
  (:import-from #:./server
                #:defhandler)
  (:import-from #:./report-api
                #:render-diff-report)
  (:import-from #:./dashboard/run-page
                #:run-row-filter
                #:row-filter
                #:mask-editor
                #:filter-selector
                #:commit)
  (:export #:diff-report
           #:render-acceptable
           #:diff-report-title
           #:make-diff-report
           #:diff-report-empty-p
           #:image-comparison-nibble
           #:render-diff-report
           #:diff-report-changes)

  ;; forward decls
  (:export #:filter-selector))


(markup:enable-reader)

(defclass diff-report ()
  ((added :initarg :added
          :initform nil)
   (deleted :initarg :deleted
            :initform nil)
   (changes :initarg :changes
            :initform nil
            :accessor diff-report-changes
            :documentation "alist of all the SCREENSHOT of old to new "
            )))

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
  (with-slots (added deleted changes) diff-report
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
  (with-slots (added deleted changes) diff-report
    (not
     (or added deleted changes))))

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
                                                                 (screenshot-masks x))))
                                                  collect (cons s1 x))))))
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

(defun image-comparison-nibble (before-image after-image)
  (with-local-image (before before-image)
    (with-local-image (after after-image)
      (uiop:with-temporary-file (:pathname p :type "png")
        (multiple-value-bind (out err ret)
            (let ((cmd (list "compare" (namestring before)
                             (namestring after)
                             (namestring p))))
              (log:info "Running: ~S" cmd)
              (uiop:run-program cmd
                                :ignore-error-status t
                                :output :interactive
                                :error-output :interactive))
          (declare (ignore out err))
          (unless (member ret '(0 1))
            (error "Got surprising error output from imagemagic compare: ~S" ret)))
        (setf (hunchentoot:header-out :content-type)
              "image/png")
        (hunchentoot:handle-static-file p)))))

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

(deftag render-diff-report (&key run to
                            (lang-filter (make-instance 'row-filter :value t))
                            (device-filter (make-instance 'row-filter :value t))
                            disable-filters
                            acceptable
                            (re-run nil))
  (flet ((filteredp (x) (and (run-row-filter lang-filter x)
                             (run-row-filter device-filter x))))
   (let ((report (make-diff-report run to))
         (next-id 0)
         (script-name (hunchentoot:script-name*))
         (github-repo (github-repo (recorder-run-channel run))))
     (with-slots (added deleted changes) report
       <markup:merge-tag>
       <div class= "page-title-box">
       ,(when acceptable
          <render-acceptable acceptable=acceptable />)
           ,(unless disable-filters
              (let ((all-runs (append added deleted (mapcar 'car changes) (mapcar 'cdr changes))))
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
         </div>
         <p class= "mt-2" >
           This commit: <commit repo= (Channel-repo (recorder-run-channel run)) hash=(recorder-run-commit run) /> <br />
           Previous commit: <commit repo= (channel-repo (recorder-run-channel run)) hash=(recorder-run-commit to) />
         </p>


       <div class= "card mt-3">
         <div class= "card-body">
           <p>
             <h1>Changes</h1>
             ,@(loop for (s . x) in changes
                     if (or (filteredp s) (filteredp x))
                       collect
                       (let* ((s s)
                              (x X)
                              (compare-nibble (nibble ()
                                                (log:info "Comparing: ~s, ~s" x s)
                                                   (image-comparison-nibble x s)))
                              (toggle-id (format nil "toggle-id-~a" (incf next-id)))
                              (modal-label (format nil "~a-modal-label" toggle-id)))
                         <div>
                           <h4 class= "d-inline-block" >
                             ,(screenshot-name s)
                           </h4> ,(progn "|")
                         <a href= "#" data-bs-toggle= "modal" data-bs-target= (format nil "#~a" toggle-id) >Compare</a>
                         ,(progn "|")
                         <a href= (nibble () (mask-editor (recorder-run-channel run) s
                            :redirect script-name))
                            >Edit Masks</a>
                           <change-image-row before-image=(image-public-url (screenshot-image x))
                                             after-image=(image-public-url (screenshot-image s))
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
                         &times;
                         </span>
                                   </button>
                                 </div>
                                 <div class="modal-body">
                                   <:img data-src= compare-nibble class= "bg-primary image-comparison-modal-image" alt= "Image Difference" />
                                 </div>
                                 <div class="modal-footer">
                                   <button type="button" class="btn btn-secondary" data-bs-dismiss="modal">Close</button>
                                 </div>
                               </div>
                             </div>
                           </div>
                         </div>))
           </p>

         </div>
       </div>

       <div class= "row">
         <div class= "col-md-6">

           <div class= "card">
             <div class= "card-body">
               <h1>Deleted</h1>
               <p>
                 ,@ (loop for c in deleted
                          if (filteredp c)
                            collect
                          <screenshot-box screenshot=c />)
               </p>

             </div>
           </div>
         </div>
       <div class= "col-md-6">
         <div class= "card">
           <div class= "card-body">
             <h1>Added</h1>
             ,@(loop for c in added
                     if (filteredp c)
                     collect
                     <screenshot-box screenshot=c />)

           </div>
         </div>
       </div>
       </div>
       </markup:merge-tag>))))

(Deftag screenshot-box (&key screenshot)
  <div>
    <h4>,(screenshot-name screenshot)</h4>
    <img class= "screenshot-box-image" src= (image-public-url (screenshot-image screenshot)) />
  </div>)
