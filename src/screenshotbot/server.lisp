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
                #:installation
                #:default-logged-in-page)
  (:import-from #:util/threading
                #:*warning-count*
                #:log-sentry)
  (:import-from #:hunchentoot-extensions
                #:log-crash-extras)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:core/ui/template
                #:*app-template*)
  (:export
   #:defhandler
   #:with-login
   #:acceptor
   #:%handler-wrap
   #:staging-p
   #:server-with-login
   #:*root*
   #:*is-localhost*
   #:document-root
   #:*reuben-ip*
   #:*disable-mail*
   #:*seleniump*
   #:*nibble-plugin*
   #:*documentation-plugin*
   #:*cdn-domain*
   #:*domain*
   #:logged-in-p
   #:dashboard
   #:error-user
   #:make-thread
   #:error-obj
   #:*landing-page*
   #:*acceptor*
   #:no-access-error
   #:prepare-acceptor-plugins
   #:error-obj
   #:error-user
   #:*seleniump*
   #:no-access-error-page
   #:*init-hooks*
   #:register-init-hook
   #:call-init-hooks)
  (:local-nicknames (#:threading #:util/threading)))
(in-package :screenshotbot/server)

(defparameter *domain* "https://screenshotbot.io")
(defvar *root* (util:system-source-directory :screenshotbot))

(defvar *is-localhost* nil)

(defvar *seleniump* nil)

(defmacro defvar-with-doc (name doc)
  `(progn
     (defvar ,name)
     (setf (documentation ',name 'variable) ,doc)))

(defvar *cdn-domain* nil
  "A CDN to use with this instance. Assuming your instance is public
  facing, and your CDN is configured to look up assets from the root
  of the domain. (e.g. https://mycdn.cloudfront.net/foo/bar.jpg should
  go to https://myscreenshotbot.xyz.com/foo/bar.jpg) Leave as NIL to
  disable CDN.")

(defsecret :trello-key
  "Trello Key used for OAuth. You must still connect your Organization
  to Trello from /settings/trello.")

(defsecret :trello-secret
  "Trello Secret used for OAuth. You must still connect your
  Organization to Trello from the /settings/trello")

(defvar *landing-page* nil
  "By default, the landing page takes you to the sign-in page if
  you're not signed in, and takes you to the dashboard when you're
  signed in. If you provide a *LANDING-PAGE* which will be a symbol
  for a function, we'll run that as the landing page instead.")

(defvar *disable-mail* nil
  "Disable emails. In the future this should be part of NOOP-MAILER,
  but this is here because we have historical code using it. We might
  be able to remove it soon.")

(defclass screenshotbot-template ()
  ())

(defun document-root ()
  (path:catdir *root* #p"static/"))

(defclass acceptor (util:base-acceptor
                    hunchensocket:websocket-acceptor
                    hex:acceptor-with-plugins) ()
  (:default-initargs
   :name 'screenshotbot-acceptor))

(defmethod hunchentoot:acceptor-request-class ((self acceptor))
  "TODO: this can be moved to :default-initargs. This is easier for
 temporary migration."
  'request)

(defclass request (hunchentoot:request)
  ())

#-screenshotbot-oss
(defclass sb-documentation-plugin (documentation-plugin:documentation-plugin)
  ())

#-screenshotbot-oss
(defmethod hex:acceptor-plugin-name ((plugin sb-documentation-plugin))
  ;; limitation of hex plugins, the routing is specified on the parent
  ;; class.
  'documentation-plugin:documentation-plugin)

(defvar *acceptor*
  (make-instance 'acceptor
                 :port 3015
                 :name 'screenshotbot-acceptor
                 :document-root (document-root)))

(defvar *init-hooks* nil)

(defun call-init-hooks ()
  (mapc #'funcall (mapcar 'cdr (reverse *init-hooks*))))

(defun register-init-hook (name function)
  (setf (alexandria:assoc-value *init-hooks* name) function))

(defvar *nibble-plugin* (make-instance 'nibble:nibble-plugin
                                        :prefix "/n/"
                                        :wrapper '%handler-wrap))

#-screenshotbot-oss
(defvar *documentation-plugin*
  (make-instance 'sb-documentation-plugin
                  :prefix "/documentation-new/"
                  :landing-page "why-screenshot-tests"
                  :root (path:catdir (util:system-source-directory :screenshotbot)
                                     #P "pro/documentation/")))

(defun prepare-acceptor-plugins (acceptor)
  (pushnew *nibble-plugin* (hex:acceptor-plugins acceptor))
  #-screenshotbot-oss
  (pushnew *documentation-plugin* (hex:acceptor-plugins acceptor)))

(prepare-acceptor-plugins *acceptor*)

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

(define-condition no-access-error (error)
  ((user :initarg :user
         :accessor error-user)
   (obj :initarg :obj
        :accessor error-obj)))

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
        (no-access-error-page)))))


(defmethod hunchentoot:start ((acceptor acceptor))
  (call-next-method))

(defmacro defhandler ((name &key uri method intern
                              want-login) params &body body)
  (multiple-value-bind (body decls) (uiop:parse-body body)
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
  (declare (optimize (speed 3))
           (type hunchentoot:request request))
  (let ((*app-template* (make-instance 'screenshotbot-template)))
   (auth:with-sessions ()
     (push-analytics-event)
     (let ((script-name (hunchentoot:script-name request))
           (util.cdn:*cdn-domain*
             (unless (staging-p)
               *cdn-domain*)))
       (cond
         ((staging-p)
          (setf (hunchentoot:header-out "Cache-Control") "no-cache"))
         ((cl-ppcre:scan *asset-regex* script-name)
          (setf (hunchentoot:header-out "Cache-Control") "max-age=3600000")))
       (when (and
              (str:starts-with-p "/assets" script-name)
              (not *is-localhost*))
         (setf (hunchentoot:header-out
                "Access-Control-Allow-Origin")
               "https://screenshotbot.io"))
       (call-next-method)))))

(defhandler (nil :uri "/force-crash") ()
  (error "ouch"))

;; (ignore-and-log-errors ()  (error "foo"))


(defhandler (nil :uri "/nibble/:nibble-id") (nibble-id)
  ;; just in case there are old nibble links lying around. Unlikely
  (hex:safe-redirect (format nil "/n/~a" nibble-id)))

(defhandler (nil :uri "/robots.txt") ()
  (Setf (hunchentoot:header-out :content-type) "text/plain")
  "User-agent: *
Disallow: /n")

(defmacro with-login ((&key (needs-login t) (signup nil)
                            (company nil)
                         (alert nil)) &body body)
  ;; server-with-login is implemented in login/common.lisp
  `(server-with-login (lambda () ,@body)
                      :needs-login ,needs-login
                      :signup ,signup
                      :company ,company
                      :alert ,alert))

(defhandler (nil :uri "/") ()
  (cond
    ((logged-in-p)
     (default-logged-in-page (installation)))
   (*landing-page*
    (funcall *landing-page*))
   (t
    (hex:safe-redirect "/login"))))

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



#+screenshotbot-oss
(setf hunchentoot-multi-acceptor:*default-acceptor*
      *acceptor*)
