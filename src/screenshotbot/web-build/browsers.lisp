;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/web-build/browsers
  (:use #:cl)
  (:nicknames :screenshotbot/pro/web-build/browsers)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/template
                #:app-template)
  (:import-from #:screenshotbot/web-build/project
                #:height
                #:browser-type
                #:mobile-emulation
                #:width
                #:browser
                #:get-browsers
                #:browser-name)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:util/form-state
                #:form-state-apply-edits
                #:make-form-state
                #:args-list-from-state
                #:form-state-initargs
                #:read-form-state
                #:form-state-class)
  (:import-from #:util/form-errors
                #:update-form-values
                #:with-form-errors)
  (:import-from #:screenshotbot/user-api
                #:current-company)
  (:import-from #:screenshotbot/web-build/device-list
                #:*device-list*)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/web-build/browsers)

(markup:enable-reader)


(defhandler (nil :uri "/browsers") ()
  <simple-card-page max-width="40em" >
    <div class= "card-header">
      <h4>Browser configurations</h4>
    </div>

    <table class= "table table-hover">
      <thead>
        <tr>
          <th>Name</th>
          <th>Type</th>
          <th>Config</th>
        </tr>
      </thead>
      <tbody>
    ,@ (loop for browser in (get-browsers)
             collect
             (util:copying (browser)
               <tr>
                 <td>
                   <a href= (nibble () (edit-browser browser))>
                     ,(browser-name browser)
                   </a>
                 </td>
                 <td>
                   ,(browser-type browser)
                 </td>
                 <td>
                   ,(cond
                      ((and (equal "chrome" (browser-type browser))
                            (not (str:emptyp (mobile-emulation browser))))
                       (mobile-emulation browser))
                      (t
                       (format nil "~ax~a" (width browser) (height browser))))
                 </td>
               </tr>))
      </tbody>
    </table>

    <div class= "card-footer">
      <a href= (nibble () (browser-form)) class= "btn btn-primary" >Add New</a>
               </div>
  </simple-card-page>)

(defun browser-form (&key object (back "/browsers"))
  <simple-card-page form-action= (nibble () (browser-form-submit :object object :back back)) >
    <div class= "card-header">
      <h4>Add new browser</h4>
    </div>
    <div class= "form-group mb-3">
      <label for= "name" class= "form-label" >Friendly name</label>
      <input type= "text" name= "name" id= "name" class= "form-control" />
    </div>

    <div class= "form-group mb-3">
      <label for= "type" class= "form-label">Browser type</label>
      <select class="form-select" name= "type" id= "type">
        <option value= "chrome">Google Chrome</option>
        <option value= "firefox">Firefox</option>
        <option value= "safari">Safari</option>
      </select>
    </div>

    <div class= "row">
      <div class= "col-6">
        <div class= "form-group mb-3">
          <label for= "width" class= "form-label">Width</label>
          <input type= "number" name= "width" id= "width" class= "form-control" />
        </div>
      </div>

      <div class= "col-6">
        <div class= "form-group mb-3">
          <label for= "height" class= "form-label">Height</label>
          <input type= "number" name= "height" id= "height" class= "form-control" />
        </div>
      </div>
    </div>

    <div class= "form-group">
      <label for= "type" class= "form-label">Mobile emulation mode (Chrome only)</label>
      <select class= "form-select" name= "mobile-emulation" id= "mobile-emulation">
        <option value= "">None</option>
        ,@ (loop for device in *device-list*
                 collect
                 <option value=device >
                   ,(progn device)
                 </option>)
      </select>
    </div>

    <div class= "card-footer">
      <input type= "submit" value= (if object "Update" "Create") class= "btn btn-primary" />
      <a class="btn btn-secondary" href=back >Back</a>
    </div>
  </simple-card-page>)

(defun if-not-empty (x)
  (unless (str:emptyp x)
    x))

(defun parse-integer-or-nil (x)
  (ignore-errors
   (parse-integer x)))

(defclass form-state ()
  ((name :mapped-to browser-name)
   (type :mapped-to browser-type)
   (width :mapped-via parse-integer-or-nil)
   (height :mapped-via parse-integer-or-nil)
   (mobile-emulation :mapped-via if-not-empty))
  (:metaclass form-state-class))

(defun browser-form-submit (&key object
                              back)
  (let ((form-state (read-form-state 'form-state)))
    (let ((errors))
      (flet ((check (symbol check message)
               (unless check
                 (push (cons symbol message) errors)))
             (safe-parse-integer (x)
               (or
                (ignore-errors
                 (parse-integer x))
                -1)))
        (with-slots (name type width height mobile-emulation) form-state
          (check :name (not (str:emptyp name))
                 "Please provide a name for this configuration")
          (check :type
                 (member type '("chrome" "firefox" "safari") :test #'equal)
                 "Invalid browser type")
          (check :mobile-emulation
                 (or
                  (str:emptyp mobile-emulation)
                  (equal "chrome" type))
                 "Mobile emulation is only supported for Chrome")
          (when (str:emptyp mobile-emulation)
            (check :width
                   (<= 1 (safe-parse-integer width) 10000)
                   "Width should be between 1 and 10000")
            (check :height
                   (<= 1 (safe-parse-integer height) 10000)
                   "Height should be between 1 and 10000"))

          (cond
            (errors
             (with-form-errors (:errors errors
                                :args-list (args-list-from-state form-state)
                                :was-validated t)
               (browser-form :object object :back back)))
            (object ;; edit
             (with-transaction ()
              (form-state-apply-edits form-state
                                      object))
             (hex:safe-redirect "/browsers"))
            (t
             (apply #'make-instance
                      'browser
                       :company (current-company)
                      (form-state-initargs form-state))
             (hex:safe-redirect "/browsers"))))))))

(defun edit-browser (browser)
  (let ((form-state (make-form-state 'form-state browser)))
    (mquerY:with-document ((browser-form :object browser))
      (update-form-values
       (args-list-from-state form-state)))))
