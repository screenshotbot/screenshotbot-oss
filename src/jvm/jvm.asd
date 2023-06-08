;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :jvm.system
  (:use #:cl
        #:asdf))
(in-package :jvm.system)

(defsystem :jvm
  :serial t
  :depends-on (:str
               :cffi
               :cl-fad
               :java.main
               (:feature :lispworks :jvm/lispcalls)
               (:feature :lispworks :deliver-utils)
               :log4cl
               :util/misc
               :util/health-check
               :util
               :trivial-garbage)
  :components ((:file "jvm")))

(defclass lispcalls-system (asdf:system)
  ())

#+lispworks
(defmethod perform ((o compile-op) (s lispcalls-system))
  (let ((output (asdf:output-file o s)))
    (uiop:copy-file (sys:lispworks-file "etc/lispcalls.jar")
                    output)))

(defmethod operation-done-p ((o compile-op) (s lispcalls-system))
  (let ((output (asdf:output-file o s)))
    (uiop:file-exists-p output)))

(defmethod asdf:output-files ((o compile-op) (s lispcalls-system))
  (list #P "lispcalls.jar"))

#+lispworks
(defsystem :jvm/lispcalls
  :class lispcalls-system)
