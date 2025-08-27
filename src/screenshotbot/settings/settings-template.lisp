;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/settings/settings-template
  (:use #:cl #:alexandria #:markup)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/template
                #:dashboard-template)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:import-from #:screenshotbot/api/core
                #:defapi)
  (:export #:settings-template #:defapi)
  (:use-reexport
   #:screenshotbot/settings-api))
(in-package :screenshotbot/settings/settings-template)

(markup:enable-reader)

(deftag settings-menu-item (children &key href)
  <li class= "nav-item" >
    <a class= (format nil "nav-link text-dark rounded ~a" (if (equal (hunchentoot:script-name*) href) "active bg-primary text-white" ""))
       href= href >
      <span class= "text">,@children </span>
    </a>
  </li>)

(defun settings-list-sections ()
  (remove-duplicates
   (loop for (nil . x) in (reverse (all-settings (installation)))
         collect (settings-section x))))

(deftag settings-render-section (&key title
                                 section)
  <markup:merge-tag>
  <li class= "nav-item settings-section-title">
    <h6 class= "text-muted text-uppercase small fw-bold py-2">,(progn title)</h6>
  </li>
  ,@ (loop for (nil . x)  in (reverse (all-settings (installation)))
           if (eql (settings-section x) section)
             collect <settings-menu-item href= (format nil "/settings/~a" (settings-name x))>,(settings-title x)</settings-menu-item>)
  </markup:merge-tag>)

(deftag settings-template (children &key title)
  (let ((sections (mapcar 'settings-section (mapcar 'cdr (all-settings (installation))))))
    (assert (equal nil
                   (set-difference (remove-duplicates sections)
                                   '(nil :vcs :tasks :organization :developers)
                                   :test #'string=)))

      <dashboard-template scripts= (list
                                    "/assets/js/settings.js")
                          title= (or title "Screenshotbot: Settings")
                      >
    
    <div class= "main-content">
      <!-- Mobile settings menu toggle -->
      <div class= "d-md-none bg-light border-bottom p-3">
        <button class= "btn btn-outline-secondary" type="button" data-bs-toggle="collapse" data-bs-target="#settingsNavbar" aria-controls="settingsNavbar" aria-expanded="false" aria-label="Toggle settings navigation">
          <i class= "bi bi-list"></i> Settings Menu
        </button>
      </div>

      <div class= "row" style= "min-height: 100vh" >
        <div class= "col-lg-3 col-md-4 collapse d-md-block" id="settingsNavbar">
          <div class= "settings-sidebar p-3 m-3">
            <h5 class= "mb-3 text-dark d-none d-md-block">Settings</h5>
            <ul class= "nav nav-pills flex-column">
              <settings-render-section title= "Account" section=nil />
              <settings-render-section title= "Organization" section=:organization />
              <settings-render-section title= "VCS" section=:vcs />
              <settings-render-section title= "Tasks" section=:tasks />
              <settings-render-section title= "Developers" section=:developers />
            </ul>
          </div>
        </div>

        <div class= "col-lg-9 col-md-8">
          <div class= "settings-content p-4 m-3">
            ,@children
          </div>
        </div>
      </div>
    </div>
  </dashboard-template>))

(defhandler (test-settings :uri "/settings") ()
  <settings-template>
    content
  </settings-template>)


(defhandler (render-single-settings-page :uri "/settings/:name"
                                         :method :get
                                         :want-login t) (name)
  (let ((installation (installation)))
   (loop for (nil . x) in (all-settings installation)
         if (string= (settings-name x)
                     name)
           do (return
                (funcall (settings-handler x)))
         finally
            (error "Could not find settings page ~a" name))))
