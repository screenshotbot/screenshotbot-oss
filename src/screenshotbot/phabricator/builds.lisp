(defpackage :screenshotbot/phabricator/builds
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/api/core
                #:defapi)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:screenshotbot/user-api
                #:current-company)
  (:import-from #:util/phabricator/conduit
                #:phab-instance
                #:call-conduit)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:bknr.datastore
                #:initialize-transient-instance)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/phabricator/builds)

(defvar *build-info-index* (make-hash-table :test #'equal))

(with-class-validation
 (defclass build-info (store-object)
   ((diff :initarg :diff
          :reader build-info-diff)
    (revision :initarg :revision
              :reader build-info-revision)
    (target-phid :initarg :target-phid
                 :accessor target-phid)
    (build-phid :initarg :build-phid
                :accessor build-phid)
    (company :initarg :company
             :reader company)
    (ts :initarg :ts
        :reader ts))
   (:metaclass persistent-class)
   (:default-initargs :ts (get-universal-time))))

(defmethod initialize-transient-instance :after ((self build-info))
  (setf (gethash (cons (company self)
                       (build-info-diff self))
                 *build-info-index*)
        self))

(defun find-build-info (company diff)
  (gethash (cons company diff) *build-info-index*))

(defapi (%update-build :uri "/phabricator/update-build" :method :post)
        ((diff :parameter-type 'integer) (revision :parameter-type 'integer) target-phid
         build-phid)
  (let ((build-info (find-build-info (current-company) diff)))
    (cond
      (build-info
       (with-transaction ()
         (setf (target-phid build-info) target-phid
               (build-phid build-info) build-phid)))
      (t
       (make-instance 'build-info
                      :diff diff
                      :revision revision
                      :target-phid target-phid
                      :build-phid build-phid
                      :company (current-company)))))
  "OK")

(defmethod sendmessage ((phab phab-instance) (self build-info) type)
  (let ((type (str:downcase type)))
    (let ((args `(("buildTargetPHID" . ,(target-phid self))
                  ("type" . ,type))))
      (call-conduit
       phab
       "harbormaster.sendmessage"
       args))))

#|
(sendmessage *phab*
(car (reverse (bknr.datastore:class-instances 'build-info)))

:fail)
|#
