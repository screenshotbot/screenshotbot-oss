;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/zookeeper
  (:use #:cl))
(in-package :screenshotbot/zookeeper)

(fli:register-module :zookeeper
                     :file-name "libzookeeper_mt.so")

(fli:define-c-struct z-handle-t)

(defun sym (name)
  (fli:dereference
   (fli:make-pointer :symbol-name (str:replace-all "-" "_" (string name))
                     :type :int)))

(defun zoo-connected-state ()
  (sym :zoo-connected-state))

(fli:define-foreign-callable ("sb_zookeeper_watcher_cb"
                              :result-type :void)
    ((zzh (:pointer :z-handle-t))
     (type :int)
     (state :int)
     (path (:pointer :char))
     (context :pointer))
  (log:info "in here ~a,~a" type state))

(fli:define-foreign-function zookeeper-init
    ((host (:reference-pass :ef-mb-string))
     (watcher-fn :pointer)
     (recv-timeout :int)
     (context :pointer)
     (flags :int))
  :result-type (:pointer :z-handle-t))

#+nil
(zookeeper-init "192.168.1.143:2181" (fli:make-pointer :symbol-name "sb_zookeeper_watcher_cb") 3000 nil 0)
