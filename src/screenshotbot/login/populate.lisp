;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/login/populate
  (:use #:cl
        #:alexandria
        #:screenshotbot/model/company
        #:screenshotbot/model/user
        #:screenshotbot/model/report
        #:screenshotbot/model/screenshot
        #:screenshotbot/model/image
        #:screenshotbot/model/recorder-run)
  (:import-from #:util #:oid)
  (:import-from #:screenshotbot/server
                #:document-root)
  (:import-from #:bknr.datastore
                #:with-transaction
                #:store-object-with-id)
  (:import-from #:screenshotbot/api/recorder-run
                #:%recorder-run-post
                #:run-response-id)
  (:import-from #:util/digests
                #:md5-file)
  (:import-from #:screenshotbot/dashboard/ensure-company
                #:populate-company)
  (:import-from #:screenshotbot/login/common
                #:after-create-user)
  (:import-from #:screenshotbot/installation
                #:multi-org-feature)
  (:import-from #:screenshotbot/model/user
                #:user-personal-company)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:util/threading
                #:ignore-and-log-errors)
  (:export #:populate-company)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/login/populate)

(defun md5sum-file (file)
  (let ((bytes (md5-file file)))
    (ironclad:byte-array-to-hex-string bytes)))


(defmethod populate-company ((company company))
  (flet ((make-image (path)
           (make-instance 'local-image
                          :hash (md5sum-file (path:catfile (document-root)
                                                               path))
                          :url (str:concat "/" (namestring path))))
         (create-run (&key image commit)
           (let ((response (%recorder-run-post
                            :channel "demo-project"
                            :company company
                            :github-repo nil
                            :screenshot-records (list
                                                 (make-instance
                                                  'dto:screenshot
                                                  :name "image1"
                                                  :image-id (oid image)))
                            :commit commit
                            :branch "master"
                            :is-trunk t
                            :is-clean t)))
             (let ((id (run-response-id response)))
               (let ((run (store-object-with-id id)))
                 (check-type run recorder-run)
                 run)))))
    (let ((image1 (make-image "assets/images/example-view-square.svg.png"))
          (image2 (make-image "assets/images/example-view.svg.png")))

      (let* ((run (create-run :image image1 :commit "abcdef1"))
             (run-2 (create-run :image image2 :commit "abcdef2")))
        (with-transaction ()
          (setf (recorder-previous-run run-2) run)
          (setf (active-run (recorder-run-channel run-2) "master") run-2))
        (let ((report
               (make-instance 'report
                               :run run-2
                               :previous-run run
                               :title "1 screenshots changed [demo]")))
          (add-company-report company report))))))

(defmethod after-create-user ((installation multi-org-feature)
                              user)
  ;; With multi-org-mode, each user will have an associated
  ;; personal company, in that case this might be a good time
  ;; to populate it with dummy data.
  (when-let ((company (user-personal-company user)))
    (ignore-and-log-errors ()
      (populate-company company))))


;; (populate-company (user-personal-company (arnold)))

;; (company-images (user-personal-company (arnold)))
