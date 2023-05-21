;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/zookeeper
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro))
(in-package :screenshotbot/zookeeper)

(fli:register-module :zookeeper
                     :file-name "libzookeeper_mt.so")

(fli:define-c-struct z-handle-t)

(defvar *next-id* 1)

(defvar *lock* (bt:make-recursive-lock))

(defvar *zks* (make-hash-table)
  "Mapping from pointer addresses to zk objects")

(defclass zk ()
  ((state :initarg :state
          :reader zk-state)))

(defun sym (name)
  (fli:dereference
   (fli:make-pointer :symbol-name (str:replace-all "-" "_" (string name))
                     :type :int)))

(defun zoo-connected-state ()
  (sym :zoo-connected-state))

(fli:define-foreign-callable ("sb_zookeeper_watcher_cb"
                              :result-type :void)
    ((zzh (:pointer z-handle-t))
     (type :int)
     (state :int)
     (path (:pointer :char))
     (context :pointer))
  (declare (ignore path context))
  (let ((zk (bt:with-recursive-lock-held (*lock*)
              (gethash (fli:pointer-address zzh) *zks*))))
   (log:info "in here ~a,~a ~a ~S" type state zk (bt:current-thread))))

(fli:define-foreign-function zookeeper-init
    ((host (:reference-pass :ef-mb-string))
     (watcher-fn :pointer)
     (recv-timeout :int)
     (context :pointer)
     (flags :int))
  :result-type (:pointer z-handle-t))

(def-easy-macro with-zookeeper (&binding zk &key host
                                         (recv-timeout 30000)
                                         (flags 0)
                                         &fn fn)
  (let ((zk (make-instance 'zk))
        zhandle-t)
    (bt:with-recursive-lock-held (*lock*)
      (setf zhandle-t (zookeeper-init
                        host
                        (fli:make-pointer :symbol-name "sb_zookeeper_watcher_cb")
                        recv-timeout
                        nil ;; context
                        flags))
      (log:info "Got zhandle: ~a ~a" zhandle-t (bt:current-thread))
      (setf (gethash (fli:pointer-address zhandle-t)
                     *zks*)
            zk))
    (unwind-protect
         (funcall fn fn)
      (log:info "Closing out zookeeper")
      (bt:with-recursive-lock-held (*lock*)
       (remhash (fli:pointer-address zhandle-t) *zks*)))))


#+nil
(zookeeper-init "192.168.1.143:2181" (fli:make-pointer :symbol-name "sb_zookeeper_watcher_cb") 3000 nil 0)

#+nil
(with-zookeeper (zk :host "192.168.1.143:2181")
  (sleep 1)
  (log:info "hello"))
