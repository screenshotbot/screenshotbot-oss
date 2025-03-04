;;;; -*- coding: utf-8 -*-
;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/template
    (:use #:cl
          #:alexandria
          #:screenshotbot/cdn
          #:screenshotbot/user-api)
  (:use-reexport #:screenshotbot/cdn
                 #:core/ui/mdi)
  (:export #:selenium-css
           #:mdi
           #:landing-head
           #:analyticsp
           #:render-landing-template
           #:google-analytics
           #:dashboard-template
           #:render-extra-scripts
           #:*favicon*
           #:left-side-bar ;; todo: defined elsewhere
           #:user-notice-list ;; todo: defined elsewhere
           #:app-template
           #:landing-template
           #:dashboard-head
           #:*template-override*
           #:mailto)
  (:import-from #:screenshotbot/installation
                #:installation-cdn
                #:installation)
  (:import-from #:screenshotbot/user-api
                #:adminp
                #:current-company)
  (:import-from #:markup
                #:deftag)
  (:import-from #:screenshotbot/server
                #:home-url
                #:screenshotbot-template
                #:acceptor
                #:defhandler
                #:staging-p
                #:*seleniump*
                #:*reuben-ip*
                #:logged-in-p)
  (:import-from #:screenshotbot/user-api
                #:user-email)
  (:import-from #:util
                #:oid)
  (:import-from #:core/ui/template
                #:render-template)
  (:import-from #:core/installation/installation
                #:site-alert
                #:installation-domain)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:util.cdn
                #:make-cdn))

(named-readtables:in-readtable markup:syntax)

(defparameter *favicon* "/assets/images/logo-favicon.png")

(defun og-image ()
  (make-cdn "/assets/images/logos/text-logo-with-vertical-space.png"))

(defmethod render-extra-scripts ((installation installation))
  nil)


(deftag selenium-css ()
  (when *seleniump*
    ;; if you change this, change this link in default.js too!
    <link rel= "stylesheet" href= "/assets/css/selenium.css" />))


(defmethod analyticsp ((self installation))
  nil)

(defmethod analyticsp :around (self)
  (and
   (boundp 'hunchentoot:*request*) ;; for tests
   (call-next-method)))

(markup:deftag google-analytics ()
  #-screenshotbot-oss
  (when (analyticsp (installation))
    <:script data-domain= (quri:uri-host (quri:uri (installation-domain (installation))))  data-api="/api/event" src="/js/script.js" defer ></:script>))

(deftag og-details ()
  <markup:merge-tag>
    <meta name="image" property="og:image" content= (og-image) />
  </markup:merge-tag>)

(deftag dashboard-head (&key jquery-ui
                        (title "Screenshotbot")
                        codemirror)
      <head>
      <meta charset="utf-8" />
      <title>,(or title "Screenshotbot")</title>
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <!-- App favicon -->
        <link rel="shortcut icon" href= (util.cdn:make-cdn *favicon*) />

        <og-details />
  ,(when jquery-ui
     <link rel= "stylesheet" href= "https://cdnjs.cloudflare.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.css" />)

  ,@(when codemirror
     (list
      <:link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.1/codemirror.min.css" />
      <:link rel= "stylesheeet"
            href= "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.1/theme/blackboard.min.css" /> ))
  <link href="/assets/css/default.css" rel="stylesheet" type="text/css" id="light-style" />
  <google-analytics />

  <selenium-css />
    </head>)

(deftag dashboard-template (children &key stripe
                            scripts
                            (title "Screenshotbot")
                            (body-class "dashboard")
                            (user (current-user))
                            (left-nav-bar t)
                            codemirror
                            (company (current-company))
                            (script-name (hunchentoot:script-name hunchentoot:*request*))
                            (left-nav-bar t)
                            jquery-ui
                            admin)
  (declare (ignore scripts))
  <html lang="en">
    <dashboard-head jquery-ui=jquery-ui codemirror=codemirror title=title />

    <body class= body-class
          data-user-id= (when user (oid user))
          data-user-email= (when user (user-email user))
          data-user-name= (when user (user-full-name user)) >
      <!-- Begin page -->
      ,(when left-nav-bar
         <left-side-bar user=user company=company script-name=script-name />)
        <div class="content-page bg-light-lighten">
          <div class="content">

            <!-- Start Content-->
  <div class="">
    ,(cond
       ((and left-nav-bar user)
        <user-notice-list user=user />)
       (t
        ;; fix this, I shouldn't need this, but it breaks the CSS if
        ;; it isn't here.
        <div id= "user-notice-list" class= "row" />))

    ,(site-alert (installation))

              ,@children
            </div> <!-- container -->

          </div> <!-- content -->


        </div>


      <!-- bundle -->


      <script src= "/assets/js/dashboard.js" />

      ,@(when codemirror
          (list
           <:script src= "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.1/codemirror.min.js" />
           <:script src= "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.1/mode/yaml/yaml.min.js" />))


      ,(when stripe
         <script src="https://js.stripe.com/v3/"></script>)


  ,(when admin
     <script src= "/assets/js/admin.js" />)
  ,(render-extra-scripts (installation))
    </body>
  </html>)

(defvar *template-override* nil)

(deftag app-template (children &key stripe
                      transparent
                      admin
                      jquery-ui
                      title
                      codemirror
                      (body-class "dashboard")
                      scripts
                      script-name
                      (nav-bar-style :dark))
  (declare (ignore nav-bar-style
                   transparent))
  (cond
    (*template-override*
     (funcall *template-override*
              children))
    (t
     <dashboard-template admin=admin jquery-ui=jquery-ui stripe=stripe scripts=scripts
                         codemirror=codemirror
                         title=title
                         body-class=body-class
                         script-name= (or script-name (hunchentoot:script-name*)) >,@children </dashboard-template>)))

(defmethod hunchentoot:acceptor-status-message ((acceptor acceptor)
                                                (http-status-code (eql hunchentoot:+http-not-found+))
                                                &rest properties
                                                &key
                                                  &allow-other-keys)
  (markup:write-html
    <landing-template>
      <section class="full-height">
        <div class= "container mt-3">

          <a href= (home-url) ><img src= "/assets/images/logo-dark.svg" style=
                            "max-height: 2em; width: auto; margin-bottom: 1em; margin-top: 2em" /></a>
          <h1>The page you're looking for doesn't exist.</h1>

          ,(progn
             #-screenshotbot-oss
             <p>If this doesn't look right, reach out at <a href="mailto:support@screenshotbot.io">support@screenshotbot.io</a>, and we'll resolve it immediately.</p>)
        </div>
      </section>
    </landing-template>))

(defmethod hunchentoot:acceptor-status-message  ((acceptor acceptor)
                                                 (http-status-code (eql 500))
                                                 &rest properties
                                                 &key &allow-other-keys)
  ;; something went wrong, let's be very careful about rendering this,
  ;; so we don't get into a secondary crash
  (cond
    ((staging-p)
     (call-next-method))
    (t
     (something-went-wrong))))

(defun something-went-wrong ()
  (let ((util.cdn:*cdn-domain*
          ;; This is outside of acceptor-dispatch-request, so we've
          ;; lost our setting for cdn-domain.
          (installation-cdn (installation))))
    (markup:write-html
     <html>
       <landing-head />
       <body>
         <section class= "error-500" >
           <div class= "container full-height">

             <h1>Oh no! Something went wrong!</h1>
             <p>We've been notified of this issue and will look into it as soon as we can.</p>

             <p>If this is blocking you, please reach out to <a href= "mailto:support@screenshotbot.io">support@screenshotbot.io</a>.</p>

             <p><a href= (home-url) >Home</a>  ,(progn "|")
             <a href= "javascript:history.back()">Back to previous page</a></p>

           </div>
         </section>
       </body>
     </html>)))

(Defhandler (get-started :uri "/get-started") ()
  (hex:safe-redirect "/documentation/getting-started"))

(deftag mailto (children)
   <a href= (format nil "mailto:~a" (car children))>,@(progn children)</a>)


(deftag landing-head (children &key
                      (title "Screenshotbot")
                      (simple nil)
                      (style))
  (setf style (or style
                  (if simple
                      "/assets/css/default.css"
                      #-screenshotbot-oss
                      "/assets/css/extended-dashboard.css"
                      #+screenshotbot-oss
                      "/assets/css/default.css")))
  <head>
    <meta charset="utf-8" />
    <title>,(progn title)</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <meta content="Build Pixel Perfect Apps with Screenshot Tests" name="description" />
      <meta content="Modern Interpreters Inc." name="author" />
      <google-analytics />
      <!-- App favicon -->
      <link rel="shortcut icon" href= (util.cdn:make-cdn *favicon*) />

      <meta name= "image" property="og:image"  content= (og-image) />
      <meta property= "og:description" content= "Build Pixel Perfect Apps with Screenshot Tests" />
      <meta property= "twitter:card" content= "summary" />
      <meta property= "twitter:site" content="@screenshotbotio" />
      <meta property= "twitter:title" content= "Build Pixel Perfect Apps with Screenshot Testing" />
      <meta property="twitter:image"  content= (og-image) />
      <meta property= "twitter:description" content= "Build Pixel Perfect Apps with Screenshot Tests" />

      <!-- App css -->
      <link href=style rel="stylesheet" type="text/css" id="light-style" />
      <selenium-css />
      ,@children
  </head>)

(defun screenshotbot/server:no-access-error-page ()
  <dashboard-template>
    <div class= "main-content">
      <div class= "card-page-container mt-3 mx-auto">
        <div class= "card">
          <div class= "card-body">
            You do not have permission to access this page. If you think this is an error please reach out to <a href= "mailto:support@screenshotbot.io">support@screenshotbot.io</a>.

            ,(when (?. adminp (auth:current-user))
               (let* ((url (hunchentoot:request-uri*))
                      (escalate (nibble ()
                                  (assert (adminp (auth:current-user)))
                                  (setf (auth:session-value :site-admin-privileges-enabled
                                                            :expires-in 3600)
                                        t)
                                  (hex:safe-redirect url))))
                 <form method= "POST" action=escalate >
                   <input type= "submit" value= "Escalate privileges"
                       class= "btn btn-danger" />
                 </form>))
          </div>
        </div>
      </div>
    </div>
  </dashboard-template>)


(deftag landing-template (body)
  (render-landing-template body))

(defmethod render-landing-template (body)
  <html>
    <landing-head />
    <body>
      ,@body
    </body>
  </html>)

(defmethod render-template ((self screenshotbot-template)
                            children &key title stripe)
  <app-template title=title stripe=stripe >
    ,@children
  </app-template>)
