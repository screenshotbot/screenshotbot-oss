;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/dashboard/history
  (:use #:cl
        #:alexandria
        #:screenshotbot/user-api
        #:screenshotbot/report-api
        #:screenshotbot/cdn
        #:screenshotbot/template
        #:screenshotbot/screenshot-api)
  (:import-from #:screenshotbot/dashboard/run-page
                #:modal-id
                #:render-modal
                #:screenshots-viewer
                #:commit)
  (:import-from #:bknr.datastore
                #:store-object-with-id)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/dashboard/run-page
                #:history-page)
  (:import-from #:core/ui/taskie
                #:timeago)
  (:import-from #:util
                #:oid)
  (:import-from #:screenshotbot/dashboard/compare
                #:comparison-modal)
  (:import-from #:core/ui/paginated
                #:paginated)
  (:import-from #:screenshotbot/user-api
                #:screenshot-name)
  (:import-from #:screenshotbot/model/image
                #:image=)
  (:import-from #:screenshotbot/model/image-comparer
                #:make-image-comparer)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/dashboard/bisect
                #:bisect-page
                #:bisect-item))
(in-package :screenshotbot/dashboard/history)

(named-readtables:in-readtable markup:syntax)

(defun render-single-screenshot (r s &key previous-screenshot
                                       channel
                                       bisect-options
                                       iterator #| the *current* iterator |#)
  (let* ((screenshots-viewer (make-instance 'screenshots-viewer
                                            :navigationp nil
                                            :screenshots (list s)))
         (image-comparer (make-image-comparer r))
         (toggle-id (format nil "compare-~a" (random 1000000)))
         (name-change-p (and
                         previous-screenshot
                         (not (string= (screenshot-name s)
                                       (screenshot-name previous-screenshot)))))
         (image-change-p (and
                          previous-screenshot
                          (not (image=
                                image-comparer
                                (screenshot-image s)
                                (screenshot-image previous-screenshot)
                                nil)))))
   (cond
     (s
      <div class= "mb-4" >
      <h6 class= "mb-0" >,(screenshot-name s)
      ,(when name-change-p
         <span class= "badge bg-warning">
           Renamed or copied
         </span>)
      </h6>

      ,(when (and previous-screenshot image-change-p)
         (comparison-modal :toggle-id toggle-id
                           :before previous-screenshot
                           :after s))
      <div class= "screenshot-header">
      <ul class= "screenshot-options-menu">
      <li>
      ,(cond
         ((recorder-run-commit r)
          <span>First seen in <commit repo= (channel-repo channel)
          hash= (recorder-run-commit r) />, ,(timeago :timestamp (created-at r)) </span>)
         (t
          <span>First seen <a href= (hex:make-url "/runs/:id" :id (oid r))>,(timeago :timestamp (created-at r))</a></span>))
      </li>

      ,(when (and previous-screenshot image-change-p)
         <li>
         <a href= "#" data-bs-toggle= "modal" data-bs-target = (format nil "#~a" toggle-id) >  Compare</a>
         </li>)

      ,(funcall bisect-options
                r s iterator)
      </ul>
      </div>
      ,(render-modal screenshots-viewer)
      <a href= (image-public-url (screenshot-image s) :size :full-page)
         class= "screenshot-run-image"
         title= "Full screenshot"
         data-image-number=0
         data-target= (format nil "#~a" (modal-id screenshots-viewer)) >
      <img class= "screenshot-image" src=(image-public-url (screenshot-image s) :size :small) />
      </a>

      </div>)
     (t
      <div>
      <h4>Deleted</h4>
      </div>))))

(markup:deftag render-history (&key screenshot-name channel branch)
  (render-history-from-iterator
   (get-screenshot-history channel screenshot-name :iterator t :branch branch)
   screenshot-name
   channel
   :bisect-options (lambda (run screenshot iterator)
                     (declare (ignore run))
                     (let ((start-bisect (nibble (:name :bisect-step-one)
                                           (start-bisect-from iterator
                                                              :screenshot-name screenshot-name
                                                              :bad-screenshot screenshot
                                                              :channel channel))))
                       <li>
                         <a href= start-bisect >Start bisect</a>
                       </li>))))

(defun start-bisect-from (iterator &key screenshot-name channel
                                     bad-screenshot)
  (app-template
   (render-history-from-iterator
    iterator
    screenshot-name
    channel
    :alert
    <div class= "alert alert-info mt-2">
      Select a <em>good</em> screenshot as a starting point for bisect
    </div>
    :bisect-options (lambda (run screenshot good-iterator)
                      (declare (ignore good-iterator))
                      (let ((start (nibble (:name :bisect-step-two)
                                     (let-the-bisect-begin iterator
                                                           :end-run run))))
                        (cond
                          ;; Is this check good enough? What happens
                          ;; when the same screenshot appears multiple
                          ;; times in the history?
                          ((fset:equal? screenshot bad-screenshot)
                           <li>
                             <span class= "text-danger">Bisect ending point (<em>bad</em> screenshot) </span>
                           </li>)
                          (t
                           <li>
                             <a href=start >Mark <em>good</em> for bisect</a>
                           </li>)))))))

(defun let-the-bisect-begin (iterator &key end-run)
  (labels ((build-bisect-list (iterator end-run &key result)
             (multiple-value-bind (args next-iter) (funcall iterator)
               (assert args)
               (destructuring-bind (screenshot run prev-run) args
                 (declare (ignore prev-run))
                 (let ((new-result (list*
                                    (make-instance 'bisect-item
                                                   :screenshot screenshot
                                                   :run run)
                                    result)))
                   (cond
                     ((eql run end-run)
                      ;; we're done
                      (reverse
                       new-result))
                     (t
                      (build-bisect-list next-iter
                                         end-run
                                         :result new-result))))))))
    (let ((bisect-list (build-bisect-list iterator end-run)))
      (bisect-page bisect-list))))

(defun render-history-from-iterator (iterator screenshot-name channel
                                     &key bisect-options
                                       alert)
  <div class= "content-page bg-light-lighten">
    <div class= "content history-page" >
      <h3 class= "mt-3 mb-0" >,(progn screenshot-name)</h3>
      ,(cond
         (alert
          alert)
         (t
          <h6 class= "text-muted mb-3">Promotion History</h6>))

      ,(paginated
        (lambda (args &key iterator)
          (destructuring-bind (screenshot run previous-screenshot)
              args
            <div>
              ,(render-single-screenshot run screenshot
                                         :previous-screenshot previous-screenshot
                                         :iterator iterator
                                         :channel channel
                                         :bisect-options bisect-options)
            </div>))

        :num 12
        :iterator iterator)
    </div>
  </div>)

(defhandler (history-page :uri "/channel/:channel/history")
            (channel screenshot-name branch)
  (when (equal "" branch)
    (setf branch nil))
  (let ((channel (store-object-with-id (parse-integer channel))))
    (can-view! channel)
    (app-template
     (render-history
      :screenshot-name screenshot-name
      :channel channel
      :branch branch))))
