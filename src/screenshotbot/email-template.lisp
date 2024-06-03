;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/email-template
  (:use #:cl)
  (:import-from #:screenshotbot/mailer
                #:smtp-mailer
                #:wrap-template
                #:mailer*
                #:send-mail)
  (:export
   #:templated-smtp-mailer))
(in-package :screenshotbot/email-template)

(named-readtables:in-readtable markup:syntax)

(markup:deftag email-template (body)
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
  <title>Your Company Name - Email</title>
  <style>
    /* You can add your own styles here */
    body {
      font-family: sans-serif;
      font-size: 16px;
      line-height: 1.5;
      margin: 0;
      padding: 0;
      color: #333;
    }
    a {
      color: #333;
      text-decoration: none;
    }
    a:hover {
      color: #007bff;
    }
    .container {
      padding: 20px;
      max-width: 600px;
      margin: 0 auto;
    }
    .header {
      display: flex;
      justify-content: center;
      align-items: center;
    }
    .logo {
      height: 50px;
      margin-bottom: 2em;
      margin-top: 1em;
    }
    .footer {
      text-align: center;
      font-size: 12px;
      margin-top: 20px;
      color: #aaa;
    background-color: #f0f0f0;;
      padding: 1em 1em;
    }
  </style>
</head>
<body>
  <div class="container">
    <header class="header">
      <table border="0" cellpadding="0" cellspacing="0" width="100%">
        <tr>
          <td align="center">
            <img src="https://cdn.screenshotbot.io/assets/images/logo-dark.png" alt="Screenshotbot" class="logo" />
          </td>
        </tr>
      </table>
    </header>

    ,@body
    <footer class="footer">
      <p>Modern Interpreters Inc, 75 Liberty Ave D1, Jersey City, NJ 07306, United States</p>
      <!-- <p><a href="[unsubscribe link]">Unsubscribe</a></p> -->
    </footer>
  </div>
</body>
</html>
)

#+nil
(send-mail
 (mailer*)
 :to "arnstein87@gmail.com"
 :subject "This is a test"
 :html-message <email-template>
    <p>hello world</p>
               </email-template>)


(defclass templated-mailer ()
  ())

(defclass templated-smtp-mailer (templated-mailer
                                 smtp-mailer)
  ())

(defmethod wrap-template ((mailer templated-mailer) html-message)
  (mquery:with-document ((call-next-method))
    (cond
      ((mquery:$ "head")
       ;; If there's a head tag, then it's already templated. The
       ;; with-document returns the original message.
       nil)
      (t
       (return-from wrap-template
         <email-template>
           ,@ (markup:xml-tag-children (car (mquery:$ "body")))
         </email-template>)))))
