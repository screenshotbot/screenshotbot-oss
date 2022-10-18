;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/settings-api
    (:use #:cl #:alexandria)
  (:import-from #:screenshotbot/installation
                #:plugin
                #:installation
                #:plugins)
  (:import-from #:screenshotbot/server
                #:staging-p)
  (:export
   #:defsettings
   #:settings
   #:all-settings
   #:settings-name
   #:settings-handler
   #:plugin-settings
   #:settings-title
   #:settings-section)
  (:export #:settings-template))
(in-package :screenshotbot/settings-api)

(defclass settings ()
  ((name :type string
         :initarg :name
         :accessor settings-name)
   (title :type string
          :initarg :title
          :accessor settings-title)
   (plugin :initarg :plugin)
   (staging-p :initarg :staging-p
              :initform nil
              :reader settings-only-staging-p)
   (section :type symbol
            :initarg :section
            :accessor settings-section)
   (handler :type (or symbol function)
            :initarg :handler
            :accessor settings-handler)))

(defvar *settings* nil)

(defmacro defsettings (name &rest body &key plugin &allow-other-keys)
  (when plugin
    (unless (eql 'quote (car plugin))
      (error "Expected form to be of type (quote foo): ~S" plugin))
    (assert (eql 2 (length plugin)))    )
  (cond
    (plugin
     `(defmethod plugin-settings ((plugin ,(cadr plugin)))
        (list
         (cons
          ',name
          (make-instance 'settings
                          ,@body)))))
    (t
     `(setf (assoc-value *settings* ',name)
            (make-instance 'settings
                            ,@body)))))

(defmethod plugin-settings ((plugin plugin))
  nil)

(defmethod all-settings ((installation installation))
  (remove-if
   (lambda (settings)
     (and
      (settings-only-staging-p (cdr settings))
      (not (staging-p))))
   (remove-if #'null
    (append
     *settings*
     (reverse
      (loop for plugin in (plugins installation)
            appending
            (plugin-settings plugin)))))))
