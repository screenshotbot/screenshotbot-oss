;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/settings-api
    (:use #:cl
          #:alexandria)
  (:import-from #:./installation
                #:plugin
                #:installation
                #:plugins)
  (:export #:defsettings
           #:settings
           #:all-settings
           #:settings-name
           #:settings-handler
           #:plugin-settings
           #:settings-title
           #:settings-section)

  ;; forward declaration
  (:export #:settings-template))

(defclass settings ()
  ((name :type string
         :initarg :name
         :accessor settings-name)
   (title :type string
          :initarg :title
          :accessor settings-title)
   (plugin :initarg :plugin)
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
  (append
   *settings*
   (reverse
    (loop for plugin in (plugins installation)
          appending
          (plugin-settings plugin)))))
