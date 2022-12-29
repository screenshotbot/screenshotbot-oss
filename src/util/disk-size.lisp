;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/disk-size
  (:use #:cl)
  (:import-from #:util/sizeof
                #:sizeof
                #:def-uint-type)
  (:local-nicknames #-lispworks
                    (:fli #:util/fake-fli)))
(in-package :util/disk-size)

(def-uint-type fsblkcnt-t
  "fsblkcnt_t"
  :imports ("sys/statvfs.h"))

(def-uint-type fsfilcnt-t
  "fsfilcnt_t"
  :imports ("sys/statvfs.h"))

(fli:define-c-struct statvfs
    (bsize :unsigned-long)
  (frsize :unsigned-long)
  (blocks fsblkcnt-t)
  (bfree fsblkcnt-t)
  (bavail fsblkcnt-t)
  (files fsfilcnt-t)
  (ffree fsfilcnt-t)
  (favail fsfilcnt-t)

  ;; There's more... but we need only until this much. Allocate using
  ;; sizeof!
  )

(defconstant +statvfs-size+
  #. (+ 8 ;; extra buf, why not
        (sizeof "struct statvfs" :imports '("sys/statvfs.h"))))


(fli:define-foreign-function (statvfs "statvfs")
    ((path (:reference-pass :ef-mb-string))
     (buf (:pointer statvfs)))
  :result-type :int)

(defun free-space (pathname)
  (fli:with-dynamic-foreign-objects ((output :char :nelems +statvfs-size+))
    (fli:with-coerced-pointer (output :type 'statvfs) output
      (statvfs (namestring pathname) output)
      (* (fli:foreign-slot-value output
                                 #-lispworks 'statvfs
                                 'bavail)
         (fli:foreign-slot-value output
                                 #-lispworks 'statvfs
                                 'bsize)))))
