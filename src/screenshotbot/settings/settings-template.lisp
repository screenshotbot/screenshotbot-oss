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
  <li class= "settings-nav-item" >
    <a class= (if (equal (hunchentoot:script-name*) href)  "active")
       href= href >,@children </a></li>)

(defun settings-list-sections ()
  (remove-duplicates
   (loop for (nil . x) in (reverse (all-settings (installation)))
         collect (settings-section x))))

(deftag settings-render-section (&key title
                                 section)
  <markup:merge-tag>
  <li class= "settings-nav-title settings-nav-item">,(progn title)</li>
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
      <div class= "row" style= "min-height: 100vh" >
        <div class= "col-md-2 border-right settings-nav-bar">
          <ul class= "settings-nav" >
            <settings-render-section title= "Account" section=nil />
            <settings-render-section title= "Organization" section=:organization />
            <settings-render-section title= "VCS" section=:vcs />
            <settings-render-section title= "Tasks" section=:tasks />
            <settings-render-section title= "Developers" section=:developers />
          </ul>
        </div>

        <div class= "col-md-10">
          <div class= "settings-conten">
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
