;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/template
  (:use :cl)
  (:import-from #:screenshotbot/cdn
                #:img)
  (:import-from #:screenshotbot/template
                #:landing-head))
(in-package :screenshotbot/login/template)

(named-readtables:in-readtable markup:syntax)

(markup:deftag auth-template (children &key body-class simple)
  <html lang= "en" >
    <landing-head>
      ,(progn
         ;; this is required in the OSS version because of it's using
         ;; a hacky mix of the pro and OSS dashboard css.
         #+screenshotbot-oss
         <style>
           html {
           font-size: 10px;
           }
         </style>)

    </landing-head>

    ,(cond
       (simple
        <body class= "">
          ,@ (progn children)
        </body>)
       (t
        <body class= (format nil "auth-pages ~a" body-class) >


          <div class= "left-image">
            <a class= "navbar-brand" href= "/">
              <img src= "/assets/images/logo-dark.png" />
            </a>
            <img class= "botty-image"  src= "/assets/images/auth/botty-left.png" />

            <span class= "copy" >
              &copy; 2018-2024 Modern Interpreters Inc.
            </span>
          </div>

          <div class= "form-container">
            <div class= "home-link">
              <a href= "/">Home</a>
            </div>
            ,@ (progn children)
          </div>
        </body>))

  </html>)