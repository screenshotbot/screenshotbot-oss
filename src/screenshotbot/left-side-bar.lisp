;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/left-side-bar
  (:use #:cl
        #:alexandria
        #:screenshotbot/user-api)
  (:import-from #:screenshotbot/installation
                #:default-oidc-provider
                #:desktop-installation
                #:installation
                #:multi-org-feature)
  (:import-from #:screenshotbot/template
                #:left-side-bar
                #:mdi)
  (:import-from #:screenshotbot/cdn
                #:img)
  (:import-from #:markup
                #:deftag)
  (:import-from #:util
                #:make-url)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/cdn
                #:img-with-fallback)
  (:import-from #:oidc/oidc
                #:logout-link)
  (:import-from #:core/ui/left-side-bar
                #:left-side-bar-container
                #:left-nav-item))
(in-package :screenshotbot/left-side-bar)

(named-readtables:in-readtable markup:syntax)

(deftag bs-icon (&key name)
  <img src= (format nil "/assets/images/icons/~a.svg" name) alt=name />)

(defmethod company-switcher ((installation multi-org-feature) &key user)
  <markup:merge-tag>
  ,@ (loop for company in (roles:companies-for-user user)
           collect
           (let ((company company))
             <li><a href= (nibble () (company-switch-page company)) class="dropdown-item">
             Switch to ,(company-name company)
             </a></li>))
     <li><a class="dropdown-item" href="/organization/new">New Organization...</a></li>

     <li>
       <hr class="dropdown-divider" />
     </li>
  </markup:merge-tag>)

(defmethod company-switcher (installation &key user)
  (declare (ignore user))
  nil)

(deftag left-side-bar (&key user company script-name)
  (declare (optimize (speed 0) (debug 3)))
  <left-side-bar-container
    logo-small-src="/assets/images/logo-small-light-scaled.webp"
    logo-src="/assets/images/logo-light-scaled.webp"
    logo-alt="Screenshotbot logo" >
    ,(render-menu-items (installation) :user user :company company :script-name script-name)
    ,(render-user-menu (installation) :user user :company company)
  </left-side-bar-container>)

(defmethod web-projects-supported-p (installation)
  t)

(defmethod billing-supported-p (installation)
  nil)

(defmethod documentation-url (installation)
  "/documentation")

(defmethod render-menu-items (installation &key user company script-name)
  <markup:merge-tag>
    <ul class="nav nav-pills flex-column ps-3 pe-3">
      <left-nav-item href= "/runs" image-class= "play_arrow"
                     script-name=script-name >
         Recent Runs
        </left-nav-item>

      ,(when (web-projects-supported-p installation)
         <left-nav-item href= "/web-projects" image-class= "cloud_queue"
                        script-name=script-name >
           Web Projects
         </left-nav-item>)

      <left-nav-item href= "/channels" image-class= "book"
                     script-name=script-name >
          Channels
      </left-nav-item>

      <left-nav-item href= "/report" image-class= "flag"
                     script-name=script-name >
                         Reports
        </left-nav-item>
    </ul>
    <hr />

    <ul class="nav nav-pills flex-column mb-auto ps-3 pe-3">
      <left-nav-item href= (documentation-url (installation)) image-class= "menu_book" target= "_blank"
                     script-name=script-name >
        Documentation
  </left-nav-item>

      ,(when (and company (not (singletonp company)))
         <left-nav-item href= "/invite" image-class= "person_add"
                        script-name=script-name >
           Invite Members
         </left-nav-item>)


      <left-nav-item href= "/api-keys" image-class= "vpn_key"
                     script-name=script-name >
        API Keys
      </left-nav-item>

      ,(when (billing-supported-p installation)
         <left-nav-item href= "/billing/dashboard" image-class= "payment"
                        script-name=script-name >
           Billing
         </left-nav-item>)

      ,(when user
         #-screenshotbot-oss
         <left-nav-item href= "/ticket/create" image-class= "report"
                        script-name=script-name >
                        Report Issue
         </left-nav-item>)
    </ul>

    <hr class= "mb-0" />
  </markup:merge-tag>)

(defmethod render-menu-items ((installation desktop-installation) &key script-name)
  <ul class="nav nav-pills flex-column ps-3 pe-3">
      <left-nav-item href= "/runs" image-class= "play_arrow"
                     script-name=script-name >
         Recent Runs
        </left-nav-item>

      <left-nav-item href= "/channels" image-class= "book"
                     script-name=script-name >
          Channels
      </left-nav-item>

      <left-nav-item href= "/report" image-class= "flag"
                     script-name=script-name >
                         Reports
        </left-nav-item>
    </ul>)

(defmethod render-user-menu (installation &key user company)
  (when user
    <div class="dropdown p-3">
      <a href="#" class="d-flex align-items-center text-white text-decoration-none dropdown-toggle" id="dropdownUser1" data-bs-toggle="dropdown" aria-expanded="false">
        <img src= (user-image-url user) alt="mdo" width="32" height="32" class="rounded-circle me-2" />
        <strong class= "user-full-name" > ,(cond
                                             ((or (not company) (singletonp company) (personalp company))
                                              (user-full-name user))
                                             (t
                                              (company-name company))) </strong>
      </a>
      <ul class="dropdown-menu dropdown-menu-dark text-small shadow multi-level" aria-labelledby="dropdownUser1">
        ,(company-switcher (installation) :user user)

        <li><a class="dropdown-item" href="/settings/general">Settings</a></li>
        ,(when (adminp user)
           <li>
	         <a href="/admin" class="dropdown-item notify-item">
	           <span>Admin</span>
	      </a>
           </li>)

        <li><a class="dropdown-item signout-link" href= (signout-link (installation)) >Sign out</a></li>
      </ul>
    </div>))

(defmethod signout-link ((self installation))
  (symbol-macrolet ((signout-link (auth:session-value :signout-link)))
   (cond
     ((default-oidc-provider self)
      (logout-link (default-oidc-provider self)))
     ((and
       (boundp 'auth:*current-session*) ;; for tests
       signout-link)
      (let ((old signout-link))
        (setf signout-link nil)
        old))
     (t
      "/signout"))))

(defmethod render-user-menu ((installation desktop-installation) &key &allow-other-keys)
  nil)
