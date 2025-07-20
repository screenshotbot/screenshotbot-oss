(defpackage :screenshotbot/github/app-installation
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:screenshotbot/github/jwt-token
                #:github-api-error-code
                #:github-api-error
                #:github-create-jwt-token
                #:github-request)
  (:import-from #:screenshotbot/github/access-checks
                #:*github-throttler*
                #:with-throttler)
  (:import-from #:screenshotbot/github/webhook
                #:*hooks*)
  (:import-from #:screenshotbot/github/plugin
                #:private-key
                #:github-plugin
                #:app-id)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:util/cron
                #:def-cron)
  (:import-from #:util/store/store-migrations
                #:def-store-migration)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:github-get-access-token-for-installation
   #:app-installed-p
   #:*repo-added-hook*))
(in-package :screenshotbot/github/app-installation)

(defclass app-installation (store-object)
  ((installation-id
    :initarg :installation-id
    :reader installation-id
    :index-type unique-index
    :index-reader app-installation-by-id)
   (repos
    :initform nil
    :accessor app-installation-repos)
   (updated-ts
    :initform nil
    :accessor updated-ts))
  (:metaclass persistent-class))

(defvar *lock* (bt:make-lock))

(defvar *repo-added-hook* nil)

(auto-restart:with-auto-restart ()
  (defun handle-webhook (json)
    (log:warn "Handle webhook is a no-op right now")))

(pushnew 'handle-webhook *hooks*)

(defun github-get-access-token-for-installation (installation-id
                                                 &key
                                                   app-id
                                                   private-key)
  (with-throttler (*github-throttler*)
   (a:assoc-value (github-request
                   (format nil "/app/installations/~a/access_tokens" installation-id)
                   :jwt-token (github-create-jwt-token
                               :app-id app-id
                               :private-key private-key)
                   :method :post)
                  :token)))

(defun app-installed-p (repo-id)
  (not (null (app-installation-id repo-id))))

(defvar *app-installation-cache* (make-hash-table :test #'equal))

(defun %app-installation-id (repo-id &key force)
  (a:assoc-value
   (flet ((%compute ()
            (block inner
              (handler-bind ((github-api-error (lambda (e)
                                                 (when (eql 404 (github-api-error-code e))
                                                   (return-from inner `((:id . nil)))))))
                (github-request
                 (format nil "/repos/~a/installation" repo-id)
                 :jwt-token (github-create-jwt-token
                             :app-id (app-id (github-plugin))
                             :private-key (private-key (github-plugin))))))))
     (cond
       (force
        (setf (gethash repo-id *app-installation-cache*)
              (%compute)))
       (t
        (util:or-setf
         (gethash repo-id *app-installation-cache*)
         (%compute)))))
   :id))

(def-cron clr-cache (:step-min 5)
  (clrhash *app-installation-cache*))


(defun app-installation-id (repo-id &key force)
  "Get the GitHub app installation id for the given
repo-id (e.g. 'tdrhq/fast-example'). If FORCE is T, then we will not
use a cached value."
  (%app-installation-id repo-id :force force))


(def-store-migration ("Delete app-installation objects -- T1963" :version 35)
  (mapc #'bknr.datastore:delete-object
        (bknr.datastore:class-instances 'app-installation)))
