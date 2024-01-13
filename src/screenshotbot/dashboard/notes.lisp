;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/notes
  (:use #:cl
        #:screenshotbot/template)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:util/form-state
                #:form-state-initargs
                #:read-form-state
                #:form-state-class)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:screenshotbot/model/note
                #:message
                #:find-notes-for
                #:note)
  (:import-from #:screenshotbot/user-api
                #:adminp
                #:user-full-name
                #:user
                #:current-user)
  (:import-from #:util/form-errors
                #:with-form-errors)
  (:import-from #:screenshotbot/impersonation
                #:make-impersonation
                #:admin-user
                #:impersonatedp
                #:impersonation)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:create-note-page
   #:render-notes))
(in-package :screenshotbot/dashboard/notes)

(markup:enable-reader)

(defun create-note-page (&rest args)
  (nibble ()
    (apply #'%create-note-page-with-impersonation-check args)))

(defclass form-state ()
  ((message :initarg :message
            :reader message))
  (:metaclass form-state-class))

(defun %create-note-page-with-impersonation-check (&rest args)
  (flet ((create-with (author)
           (apply '%create-note-page
                   :author author
                   args)))
   (let ((impersonation (make-impersonation)))
     (cond
       ((impersonatedp impersonation)
        (let ((admin-user (admin-user impersonation)))
          (assert admin-user)
          <app-template>
            (internal only) Who should we create this as?
            <ul>
              <li>
                <a href= (nibble () (create-with admin-user))>,(user-full-name admin-user)</a>
              </li>
              <li>
                <a href= (nibble () (create-with (current-user)))>,(user-full-name (current-user))</a>
              </li>
            </ul>
          </app-template>))
       (t
        (create-with (current-user)))))))

(defun %create-note-page (&key for redirect author)
  (assert redirect)
  <simple-card-page form-action= (nibble () (submit-create :for for :redirect redirect :author author)) >
      <div class= "card-header">
        <h4>Add a note</h4>
      </div>
      <div class= "form-group">
        <label for= "message" class= "form-label" >Message</label>
        <textarea id= "message" name= "message" class= "form-control mb-3" />
      </div>
      <div class= "card-footer">
        <input type= "submit" class= "btn btn-primary" value= "Done" />
      </div>
  </simple-card-page>)

(defun submit-create (&key for redirect author)
  (assert redirect)
  (let ((form-state (read-form-state 'form-state))
        errors)
    (with-slots (message) form-state
     (flet ((check (field test message)
              (unless test
                (push (cons field message) errors))))
       (check :message (not (str:emptyp message))
              "Please provide a message")
       (cond
         (errors
          ;; TODO: if we ever do an edit, look at form-state-validate
          (with-form-errors (:errors errors
                             :message message
                             :was-validated t)
            (%create-note-page :for for :redirect redirect)))
         (t
          (let ((note (apply #'make-instance 'note
                               :for for
                               :user author
                               (form-state-initargs form-state))))
            (declare (ignore note))
            (hex:safe-redirect redirect))))))))

(defun render-user-name (user)
  (cond
    #-screenshotbot-oss
    ((adminp user)
     "Screenshotbot Support")
    (t
     (user-full-name user))))

(defun render-notes (&key for)
  (let ((notes (find-notes-for for)))
    (when notes
      <div class= "mt-3" >
        ,@ (loop for note in notes collect
        <div class= "alert alert-info mt-1" >
          <h6><b>,(render-user-name (user note))</b> added a note:</h6>
          <div>,(message note)</div>
        </div>)
      </div>)))
