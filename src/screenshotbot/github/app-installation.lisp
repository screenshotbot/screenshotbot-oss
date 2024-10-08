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
 (defun update-app-installation (installation-id)
   (let ((app-installation (bt:with-lock-held (*lock*)
                             (or
                              (app-installation-by-id installation-id)
                              (make-instance 'app-installation
                                              :installation-id installation-id))))
         (github-plugin (github-plugin)))
     (let ((access-token (github-get-access-token-for-installation
                          (installation-id app-installation)
                          :app-id
                          (app-id github-plugin)
                          :private-key (private-key github-plugin))))
       (let ((repos (loop for page from 0 to 100
                          for json-response = (with-throttler (*github-throttler*)
                                                (github-request
                                                 (format nil
                                                         "/installation/repositories?page=~d" page)
                                                 :installation-token access-token))
                          for repos = (a:assoc-value json-response :repositories)
                          while repos
                          appending
                          (loop for repo in repos
                                collect (a:assoc-value repo :full--name))
                          do
                             (setf json-response ()))))
         (let ((old-repos (app-installation-repos app-installation)))
          (with-transaction ()
            (setf (app-installation-repos app-installation)
                  repos)
            (setf (updated-ts app-installation) (get-universal-time)))
           (dolist (repo (set-difference repos old-repos :test #'equal))
             (dolist (hook *repo-added-hook*)
               (funcall hook repo)))))))))

(defun delete-app-installation (installation-id)
  (let ((installation (app-installation-by-id installation-id)))
    (when installation
     (bknr.datastore:delete-object
      installation))))

(auto-restart:with-auto-restart ()
  (defun handle-webhook (json)
    (log:info "handle-webhook being called")
    (let ((installation (a:assoc-value json :installation)))
      (when installation
        (let ((action (a:assoc-value json :action))
              (installation-id (a:assoc-value installation :id)))
          (when (or
                 (equal "created" action)
                 (a:assoc-value json :repositories--added)
                 (a:assoc-value json :repositories--removed))
            (log:info "Updating app installation")
            (update-app-installation installation-id))
          (when (equal "deleted" action)
            (delete-app-installation installation-id)))))))

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
