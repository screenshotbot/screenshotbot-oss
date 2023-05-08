;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-layout
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/layout
                #:layout-with-hash
                #:layout))
(in-package :screenshotbot/model/test-layout)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test simple-layout-finder ()
  (with-fixture state ()
    (let ((company-1 (make-instance 'company))
          (company-2 (make-instance 'company)))
      (let ((layout-1 (make-instance 'layout :company company-1
                                     :hash #(1 2 3)))
            (layout-2 (make-instance 'layout :company company-2
                                             :hash #(1 2 3)))
            (layout-3 (make-instance 'layout :company company-1
                                             :hash #(1 2 3))))
        (is (eql layout-1
             (layout-with-hash
              company-1
              #(1 2 3))))
        (is (eql layout-2
                 (layout-with-hash
                  company-2
                  #(1 2 3))))))))
