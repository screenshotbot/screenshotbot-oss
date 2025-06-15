;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/uname
  (:use #:cl)
  (:nicknames :%uname)
  (:import-from #:alexandria
                #:assoc-value))
(in-package :screenshotbot/uname)

(defclass uname ()
  ((str :initarg :str
        :reader uname-str)
   (os :initarg :os
       :reader uname-os)
   (kernel-version
    :initarg :kernel-version
    :reader uname-kernel-version)))

(defclass linux-uname (uname)
  ())

(defclass darwin-uname (uname)
  ())

(defmethod parse-os ((uname string))
  (cond
    ((str:starts-with-p "Darwin" uname)
     "Darwin")
    ((str:starts-with-p "Linux" uname)
     "Linux")
    (t
     "Other")))

(defmethod %scan (index regex string)
  "If the regex matchers, return the index-ed part of the matching"
  (multiple-value-bind (res parts)
      (cl-ppcre:scan-to-strings regex string)
    (when res
      (elt parts index))))

(defmethod %scan (index regex (uname uname))
  (%scan index regex (uname-str uname)))

(defmethod darwinp ((uname string))
  (equal "Darwin" (parse-os uname)))

(defmethod linuxp ((uname string))
  (equal "Linux" (parse-os uname)))

(defmethod parse-kernel-version ((uname string))
  (cond
    ((darwinp uname)
     (%scan 0 "Darwin .*? Darwin Kernel Version (.*): " uname))
    ((linuxp uname)
     (%scan 0 "Linux .*? ([1-9].*?) " uname))))


(defmethod parse-uname ((uname string))
  "Parses the output of `uname -a`"
  (make-instance (cond
                   ((equal "Darwin" (parse-os uname))
                    'darwin-uname)
                   ((equal "Linux" (parse-os uname))
                    'linux-uname)
                   (t
                    'uname))
                 :os (parse-os uname)
                 :str uname
                 :kernel-version (parse-kernel-version uname)))

(defgeneric uname-arch (uname)
  (:method (uname)
    nil)
  (:method ((uname darwin-uname))
    (car (last (str:split " " (uname-str uname)))))
  (:method ((uname linux-uname))
    ;; Technically, the man page says "non-portable". 
    (second (reverse (str:split " " (uname-str uname))))))

;; This list was generated with the help of Gemini from the table
;; https://en.wikipedia.org/wiki/Apple_silicon#Comparison_of_M-series_processors
;; I first used the source of that table and asked Gemini to create a
;; JSON map. Then asked it to create a common lisp alist out of it.
(defparameter *processor-types*
  `(("T8103" . "M1")
    ("T6000" . "M1 Pro")
    ("T6001" . "M1 Max")
    ("T6002" . "M1 Ultra")
    ("T8112" . "M2")
    ("T6020" . "M2 Pro")
    ("T6021" . "M2 Max")
    ("T6022" . "M2 Ultra")
    ("T8122" . "M3")
    ("T6030" . "M3 Pro")
    ("T6034" . "M3 Max")
    ("T6031" . "M3 Max")
    ("T6032" . "M3 Ultra")
    ("T8132" . "M4")
    ("T6040" . "M4 Pro")
    ("T6041" . "M4 Max")

    ;; This one was added manually
    ("VMAPPLE" . "VMAPPLE")))

(defgeneric parse-t-type (uname)
  (:method (uname)
    nil)
  (:method ((uname darwin-uname))
    (%scan 0 "RELEASE_.*?_(T[0-9]*|VMAPPLE) " uname)))

(defgeneric cpu-codename (uname)
  (:method (uname)
    nil)
  (:method ((uname darwin-uname))
    (let ((result (assoc-value
                   *processor-types*
                   (parse-t-type uname)
                   :test #'string-equal)))
      (unless result
        (warn "unknown cpu codename: ~a" (uname-str uname)))
      result)))
