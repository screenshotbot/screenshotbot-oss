;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/server
    (:use :cl :alexandria)
  (:import-from #:screenshotbot/analytics
                #:push-analytics-event)
  (:import-from #:screenshotbot/secret
                #:defsecret)
  (:import-from #:screenshotbot/installation
                #:installation-cdn
                #:installation
                #:default-logged-in-page)
  (:import-from #:util/threading
                #:with-tags
                #:*warning-count*
                #:log-sentry)
  (:import-from #:hunchentoot-extensions
                #:log-crash-extras)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:core/ui/template
                #:*app-template*)
  (:import-from #:core/installation/installation
                #:installation-domain)
  #+lispworks
  (:import-from #:hunchentoot-extensions/existing-socket
                #:acceptor-with-existing-socket)
  #+ (and lispworks linux)
  (:import-from #:bknr.cluster/server
                #:leaderp)
  (:import-from #:util/throttler
                #:throttled-error)
  (:import-from #:auth
                #:no-access-error
                #:logged-in-p)
  (:import-from #:screenshotbot/login/common
                #:with-login)
  (:import-from #:core/api/acceptor
                #:api-acceptor-mixin)
  (:import-from #:core/ui/fonts
                #:fonts-acceptor-mixin)
  (:import-from #:core/active-users/active-users
                #:mark-active-user)
  (:import-from #:util/events
                #:push-event)
  (:import-from #:core/installation/request
                #:with-installation-for-request)
  (:export
   #:defhandler
   #:with-login
   #:acceptor
   #:%handler-wrap
   #:staging-p
   #:*root*
   #:*is-localhost*
   #:document-root
   #:*reuben-ip*
   #:*seleniump*
   #:*domain*
   #:logged-in-p
   #:dashboard
   #:make-thread
   #:*acceptor*
   #:prepare-acceptor-plugins
   #:*seleniump*
   #:no-access-error-page
   #:*init-hooks*
   #:register-init-hook
   #:call-init-hooks
   #:request
   #:redirect-home
   #:home-url)
  (:local-nicknames (#:threading #:util/threading)))
(in-package :screenshotbot/server)

(defparameter *domain* "https://screenshotbot.io")
(defvar *root* (util:relative-system-source-directory :screenshotbot))

(defvar *is-localhost* nil)

(defvar *seleniump* nil)

(defmacro defvar-with-doc (name doc)
  `(progn
     (defvar ,name)
     (setf (documentation ',name 'variable) ,doc)))

(defsecret :trello-key
  "Trello Key used for OAuth. You must still connect your Organization
  to Trello from /settings/trello.")

(defsecret :trello-secret
  "Trello Secret used for OAuth. You must still connect your
  Organization to Trello from the /settings/trello")

(defclass screenshotbot-template ()
  ())

(defun document-root ()
  (path:catdir *root* #p"static/"))

(defclass acceptor (#+lispworks acceptor-with-existing-socket
                    nibble:nibble-acceptor-mixin
                    auth:auth-acceptor-mixin
                    api-acceptor-mixin
                    hex:clos-dispatcher
                    util:base-acceptor
                    fonts-acceptor-mixin
                    hunchensocket:websocket-acceptor
                    hex:acceptor-with-plugins) ()
  (:default-initargs
   :name 'screenshotbot-acceptor
   :document-root (document-root)
   :request-class 'request))

(defclass request (auth:authenticated-request
                   hunchensocket::websocket-request)
  ())

(defmethod auth:authenticate-request :after ((request request))
  (mark-active-user :user (auth:request-user request)
                    :company (auth:request-account request)))

(defvar *acceptor*
  (make-instance 'acceptor
                 :port #-screenshotbot-oss 4001
                       #+screenshotbot-oss 4091 ;; Called directly from launch.lisp
                 :name 'screenshotbot-acceptor
                 :document-root (document-root)))

(defvar *init-hooks* nil)

(defun call-init-hooks ()
  (mapc #'funcall (mapcar 'cdr (reverse *init-hooks*))))

(defun register-init-hook (name function)
  (setf (alexandria:assoc-value *init-hooks* name) function))

(defmethod nibble:nibble-funcall :around ((self acceptor) nibble)
  (%handler-wrap
   (lambda ()
     (call-next-method))))

;; (hunchentoot:start *acceptor*)

(defun staging-p ()
  (declare (optimize (speed 3)))
  (cond
    (t
     (or
      (not (boundp 'hunchentoot:*request*))
      (let ((host (hunchentoot:host)))
        (or
         #+screenshotbot-oss
         t
         (str:starts-with-p "192.168.1.119" host)
         (str:starts-with-p "localhost" host)
         (str:starts-with-p "staging." host)))))))

(defun pp (x)
  (log:info "~S" x)
  x)

(def-easy-macro with-sentry-extras (&fn fn)
  (let ((threading:*extras*
              (list*
               (lambda (e)
                 (log-crash-extras hunchentoot:*acceptor* e))
               threading:*extras*)))
    (funcall fn)))

(defun %handler-wrap (impl)
  (with-sentry-extras ()
    (handler-case
        (funcall impl)
      (no-access-error (e)
        (push-event :no-access-error)
        (no-access-error-page))
      (throttled-error (e)
        (declare (ignore e))
        (setf (hunchentoot:return-code*) 429)
        (warn "Too many request: ~a" e)
        "Too many requests"))))

(defmacro defhandler ((name &key uri method intern
                              want-login) params &body body)
  (multiple-value-bind (body decls) (uiop:parse-body body :documentation t)
    `(util:better-easy-handler (,name :uri ,uri :method ,method :acceptor-names '(screenshotbot-acceptor)
                                      :intern ,intern)
        ,params
        ,@ decls
       (%handler-wrap (lambda ()
                        ,@ (if want-login
                               (list
                                `(with-login ()
                                   ,@body))
                               body))))))

(defparameter *asset-regex*
  (cl-ppcre:create-scanner "[.](js|css|woff|otf|woff2|png|jpg|jpeg|webp|svg)$"))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor acceptor) request)
  #+bknr.cluster
  (when (and
         (boundp 'bknr.datastore:*store*)
         (not (leaderp bknr.datastore:*store*)))
    ;; If we're here, it's probably because the leader got transfered
    ;; recently. The new leader might not be ready yet, so this hack
    ;; slows down nginx before it retries the next backend.
    (sleep 3)
    (cond
      ((leaderp bknr.datastore:*store*)
       ;; If we've become the leader while we were waiting then just
       ;; continue.
       (values))
      (t
       (setf (hunchentoot:return-code*) 502)
       (hunchentoot:abort-request-handler))))
  (with-installation-for-request (request)
   (with-tags (("hostname" (uiop:hostname)))
     (let ((*app-template* (make-instance 'screenshotbot-template)))
       (auth:with-sessions ()
         (push-analytics-event)
         (let ((script-name (hunchentoot:script-name request))
               (util.cdn:*cdn-domain*
                 (installation-cdn (installation))))
           (cond
             ((staging-p)
              (setf (hunchentoot:header-out "Cache-Control") "no-cache"))
             ((cl-ppcre:scan *asset-regex* script-name)
              (setf (hunchentoot:header-out "Cache-Control") "max-age=3600000")))
           (setf (hunchentoot:header-out "X-Frame-Options") "DENY")
           (when (and
                  (str:starts-with-p "/assets" script-name)
                  (not *is-localhost*))
             (setf (hunchentoot:header-out
                    "Access-Control-Allow-Origin")
                   (installation-domain (installation))))
           (call-next-method)))))))

(defhandler (nil :uri "/force-crash") ()
  (error "ouch"))

(hex:def-clos-dispatch ((self acceptor) "/force-crash2") ()
  (error "ouch2"))

;; (ignore-and-log-errors ()  (error "foo"))


(defhandler (nil :uri "/nibble/:nibble-id") (nibble-id)
  ;; just in case there are old nibble links lying around. Unlikely
  (hex:safe-redirect (format nil "/n/~a" nibble-id)))

(defhandler (nil :uri "/robots.txt") ()
  (Setf (hunchentoot:header-out :content-type) "text/plain")
  (cond
    ((staging-p)
     "User-agent: *
Disallow: /")
    (t
     "User-agent: *
Disallow: /n
Disallow: /runs/by-tag
Disallow: /badge
Disallow: /active-run
")))

(defhandler (nil :uri "/") ()
  (cond
    ((logged-in-p)
     (default-logged-in-page (installation)))
    (t
     (render-landing-page (installation)))))

(defmethod render-landing-page ((self installation))
  (hex:safe-redirect "/login"))

(defun home-url ()
  (if (logged-in-p)
      "/runs"
      "/"))

(defun redirect-home ()
  (hex:safe-redirect "/runs"))

(server:register-acceptor *acceptor*
                          "screenshotbot.io"
                          "staging.screenshotbot.io"
                          "kickstarter.screenshotbot.io"
                          "mx.tdrhq.com"
                          "api.screenshotbot.io")

(defun make-thread (fn &rest args)
  (apply
   'util:make-thread
   fn
   args))

(defhandler (nil :uri "/test-headers") ()
  (format nil ":~a:" (hunchentoot:header-in* :x-forwarded-proto)))


(defhandler (nil :uri "/test-nibble-redirect") ()
  (hex:safe-redirect
   (nibble:nibble ()
     "hello world")))

(defhandler (nil :uri "/confirm-email/:id/:code" :method :get)
            (code id)
  (hex:safe-redirect "/confirm-email"
                     :id id :code code))
