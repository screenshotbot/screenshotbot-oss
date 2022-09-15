;;;; -*- coding: utf-8 -*-
;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/template
    (:use #:cl
          #:alexandria
          #:./cdn
          #:./user-api)
  (:use-reexport #:./cdn)
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
           #:*template-override*)
  (:import-from #:./installation
                #:installation)
  (:import-from #:./user-api
                #:current-company)
  (:import-from #:markup
                #:deftag)
  (:import-from #:./server
                #:acceptor
                #:defhandler
                #:staging-p
                #:*seleniump*
                #:*reuben-ip*
                #:logged-in-p)
  (:import-from #:screenshotbot/user-api
                #:user-email)
  (:import-from #:util
                #:oid))

(markup:enable-reader)

(defparameter *favicon* "/assets/images/logo-favicon.png")

(defparameter *og-image*
  "https://screenshotbot.io/assets/images/logo-dark-scaled.webp")

(defmethod render-extra-scripts ((installation installation))
  nil)


(deftag mdi (&key name class)
  <i class= (format nil "material-icons ~a" class) >,(progn name)</i>)

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
    <:script data-domain="screenshotbot.io" src="/js/script.js" data-api= "/api/event" defer ></:script>))

(deftag dashboard-head (&key jquery-ui
                        (title "Screenshotbot")
                        codemirror)
      <head>
      <meta charset="utf-8" />
      <title>,(or title "Screenshotbot")</title>
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <!-- App favicon -->
        <link rel="shortcut icon" href= (util.cdn:make-cdn *favicon*) />

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
                            codemirror
                            (company (current-company))
                            (script-name (hunchentoot:script-name hunchentoot:*request*))
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
      <left-side-bar user=user company=company script-name=script-name />
        <div class="content-page bg-light-lighten">
          <div class="content">

            <!-- Start Content-->
  <div class="">
  <user-notice-list user=user />
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
                      scripts
                      script-name
                      (nav-bar-style :dark))
  (declare (ignore nav-bar-style
                   transparent))
  (cond
    (*template-override*
     (funcall *template-override*
              children))
    ((logged-in-p)
     <dashboard-template admin=admin jquery-ui=jquery-ui stripe=stripe scripts=scripts
                         codemirror=codemirror
                         title=title
                         script-name= (or script-name (hunchentoot:script-name*)) >,@children </dashboard-template>)
    (t
     (Assert (not admin))
     <html>
       <landing-head />
       <section>
         <div class= "container">
           ,@children
         </div>
       </section>
     </html>)))

(defmethod hunchentoot:acceptor-status-message ((acceptor acceptor)
                                                (http-status-code (eql hunchentoot:+http-not-found+))
                                                &rest properties
                                                &key
                                                  &allow-other-keys)
  (markup:write-html
    <landing-template>
      <section class="full-height">
        <div class= "container">
          <h1>The page you're looking for doesn't exist.</h1>
          <p>If this doesn't look right, mail me at <a href="mailto:arnold@screenshotbot.io">arnold@screenshotbot.io</a>, and I'll resolve it immediately.</p>
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
  (markup:write-html
   <html>
   <landing-head />
   <body>
   <section class= "error-500" >
   <div class= "container full-height">

   <h1>Oh no! Something went wrong!</h1>
   <p>We've been notified of this issue and will look into it as soon as we can.</p>

   <p>If this is blocking you, please reach out to <a href= "mailto:support@screenshotbot.io">support@screenshotbot.io</a>.</p>

   <p><a href= "/">Home</a>  ,(progn "|")
   <a href= "javascript:history.back()">Back to previous page</a></p>

        </div>
   </section>
   </body>
   </html>))

(Defhandler (get-started :uri "/get-started") ()
  (hex:safe-redirect "/documentation/getting-started"))


(deftag landing-head (&key
                      (style #-screenshotbot-oss
                             "/assets/css/extended-dashboard.css"
                             #+screenshotbot-oss
                             "/assets/css/default.css"))
  <head>
    <meta charset="utf-8" />
    <title>Screenshotbot</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <meta content="Build Pixel Perfect Apps with Screenshot Tests" name="description" />
      <meta content="Modern Interpreters Inc." name="author" />
      <google-analytics />
      <!-- App favicon -->
      <link rel="shortcut icon" href= (util.cdn:make-cdn *favicon*) />

      <meta property="og:image"  content= *og-image* />
      <meta property= "og:description" content= "Build Pixel Perfect Apps with Screenshot Tests" />
      <meta property= "twitter:card" content= "summary" />
      <meta property= "twitter:site" content="@screenshotbotio" />
      <meta property= "twitter:title" content= "Build Pixel Perfect Apps with Screenshot Testing" />
      <meta property="twitter:image"  content= *og-image* />
      <meta property= "twitter:description" content= "Build Pixel Perfect Apps with Screenshot Tests" />

      <!-- App css -->
      <link href=style rel="stylesheet" type="text/css" id="light-style" />
      <selenium-css />
  </head>)

(defun screenshotbot/server:no-access-error-page ()
  <html>
    <landing-head />
    <body>
      <section>
        <div class= "container full-height">
          You do not have permission to access this page. If you think this is an error please reach out to <a href= "mailto:support@screenshotbot.io">support@screenshotbot.io</a>.
        </div>
      </section>
    </body>
  </html>)


(deftag landing-template (body)
  (render-landing-template body))

(defmethod render-landing-template (body)
  <html>
    <landing-head />
    <body>
      ,@body
    </body>
  </html>)
