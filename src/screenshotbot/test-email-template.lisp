;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-email-template
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:screenshot-test)
  (:import-from #:screenshotbot/email-template
                #:templated-mailer
                #:email-template)
  (:import-from #:screenshotbot/mailer
                #:wrap-template))
(in-package :screenshotbot/test-email-template)

(util/fiveam:def-suite)

(named-readtables:in-readtable markup:syntax)

(def-fixture state ()
  (with-installation ()
    (&body)))

(screenshot-test basic-email-template-test
  (with-fixture state ()
    <email-template>
      <p>Hello world!</p>
    </email-template>))


(test wrap-template-for-email-template
  (with-fixture state ()
    (let ((mailer (make-instance 'templated-mailer)))
      (finishes
        (wrap-template
         mailer
         <html>
           <body>hello</body>
         </html>))
      (finishes
        (wrap-template
         mailer
         <html>
           <head></head>
           <body>hello</body>
         </html>))
      (finishes
        (wrap-template
         mailer
         <html>
           <head />
         </html>)))))
