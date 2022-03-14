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
  (:export
   #:defhandler
   #:with-login
   #:acceptor
   #:%handler-wrap
   #:staging-p
   #:server-with-login
   #:*root*
   #:*is-localhost*
   #:init-for-delivered-image
   #:document-root
   #:*reuben-ip*
   #:*disable-mail*
   #:*seleniump*
   #:*nibble-plugin*
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
   #:*init-hooks*))
(in-package :screenshotbot/server)

(defparameter *domain* "https://screenshotbot.io")
(defvar *root* (util:system-source-directory :screenshotbot))

(defvar *is-localhost* nil)
(defvar *reuben-ip* "") ;; see update-reuben-ip.lisp
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

(defun document-root ()
  (path:catdir *root* #p"static/"))

(defclass acceptor (util:base-acceptor
                    hunchensocket:websocket-acceptor
                    hex:acceptor-with-plugins) ())

(defvar *acceptor*
  (make-instance 'acceptor
                 :port 3015
                 :name 'screenshotbot-acceptor
                 :document-root (document-root)))

(defvar *init-hooks* nil)

(defun init-for-delivered-image ()
  (setf *root* (pathname "~/web/src/screenshotbot/"))
  (setf (hunchentoot:acceptor-document-root
         *acceptor*)
        (document-root))
  (mapc #'funcall *init-hooks*))

(defvar *nibble-plugin* (make-instance 'nibble:nibble-plugin
                                        :prefix "/n/"
                                        :wrapper '%handler-wrap))

(defun prepare-acceptor-plugins (acceptor)
 (setf (hex:Acceptor-plugins acceptor)
       (list *nibble-plugin*)))

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

(defun %handler-wrap (impl)
  (restart-case
      (let ((util.cdn:*cdn-domain*
              (unless (staging-p)
                *cdn-domain*)))
        (auth:with-sessions ()
          (push-analytics-event)
          (handler-case
              (funcall impl)
            (no-access-error (e)
              (no-access-error-page)))))
    (retry-handler ()
      (%handler-wrap impl))))


(defmethod hunchentoot:start ((acceptor acceptor))
  (call-next-method))

(defmacro defhandler ((name &key uri method intern (html t)
                              want-login) params &body body)
  (multiple-value-bind (body decls) (uiop:parse-body body)
    `(util:better-easy-handler (,name :uri ,uri :method ,method :acceptor-names '(screenshotbot-acceptor)
                                      :intern ,intern
                                      :html ,html)
        ,params
        ,@ decls
       (%handler-wrap (lambda ()
                        ,@ (if want-login
                               (list
                                `(with-login ()
                                   ,@body))
                               body))))))

(defparameter *asset-regex*
  (cl-ppcre:create-scanner "[.](js|css|woff|otf|woff2|png|jpg|jpeg|webp)$"))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor acceptor) request)
  (declare (optimize (speed 3))
           (type hunchentoot:request request))
  (let ((script-name (hunchentoot:script-name request)))
    (cond
      ((staging-p)
       (setf (hunchentoot:header-out "Cache-Control") "no-cache"))
      ((cl-ppcre:scan *asset-regex* script-name)
       (setf (hunchentoot:header-out "Cache-Control") "max-age=360000")))
    (when (and
           (str:starts-with-p "/assets" script-name)
           (not *is-localhost*))
      (setf (hunchentoot:header-out
             "Access-Control-Allow-Origin")
            "https://screenshotbot.io"))
    (call-next-method)))

(defhandler (nil :uri "/force-crash") ()
  (error "ouch"))


;; (ignore-and-log-errors ()  (error "foo"))


(defhandler (nil :uri "/nibble/:nibble-id") (nibble-id)
  ;; just in case there are old nibble links lying around. Unlikely
  (hex:safe-redirect (format nil "/n/~a" nibble-id)))

(defhandler (nil :uri "/robots.txt" :html nil) ()
  (Setf (hunchentoot:header-out :content-type) "text/plain")
  "User-agent: *
Disallow: /n")

(defmacro with-login ((&key (needs-login t)) &body body)
  ;; server-with-login is implemented in login/common.lisp
  `(server-with-login (lambda () ,@body)
                      :needs-login ,needs-login))

(defhandler (nil :uri "/") ()
  (cond
   ((logged-in-p)
    (hex:safe-redirect "/dashboard"))
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
   'bt:make-thread
   (lambda ()
     (handler-bind ((error (lambda (e)
                             #-screenshotbot-oss
                             (when hunchentoot:*catch-errors-p*
                              (sentry-client:capture-exception e)))))
       (funcall fn)))
   args))

#+screenshotbot-oss
(setf hunchentoot-multi-acceptor:*default-acceptor*
      *acceptor*)
