;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :scheduled-jobs/bindings
  (:use #:cl)
  (:import-from #:local-time
                #:unix-to-timestamp
                #:timestamp-to-unix)
  (:import-from #:util/health-check
                #:def-health-check)
  (:local-nicknames (#:a #:alexandria)
                    #-lispworks
                    (#:fli #:util/fake-fli))
  (:import-from #:util/native-module
                #:load-module
                #:define-embedded-module
                #:make-native-module)
  (:export
   #:cron-parse-expr
   #:cron-next
   #:invalid-cron-expr))
(in-package :scheduled-jobs/bindings)

(defvar *native-module* (make-native-module 'ccronexpr
                                            :scheduled-jobs
                                            "ccronexpr"))

(define-embedded-module *native-module*)

(fli:define-c-struct fli-cron-expr
    (seconds (:c-array :uint8 8))
  (minutes (:c-array :uint8 8))
  (hours (:c-array :uint8 3))
  (days-of-week (:c-array :uint8 1))
  (days-of-month (:c-array :uint8 4))
  (months (:c-array :uint8 2)))

(fli:define-foreign-function (%cron-next "cron_next")
  ((expr (:pointer fli-cron-expr))
   (date :time-t))
  :result-type :time-t)

(fli:define-foreign-function (%cron-parse-expr "cron_parse_expr")
    ((expr (:reference-pass :ef-mb-string))
     (target (:pointer fli-cron-expr))
     (err (:pointer (:pointer :char))))
  :result-type :void)

(define-condition invalid-cron-expr (error)
  ((message :initarg :message
            :reader invalid-cron-expr-message)))

(defmethod print-object ((e invalid-cron-expr) out)
  (format out "#<CRON ~a>" (invalid-cron-expr-message e)))

(defclass cron-expr ()
  ((fli-cron-expr :initarg :fli-cron-expr
                  :reader fli-cron-expr)))


(defun cron-parse-expr (expr)
  (load-module *native-module*)
  (fli:with-dynamic-foreign-objects ((err (:pointer :char) :fill 0))
   (let ((fli-cron-expr (fli:malloc :type 'fli-cron-expr)))
     (%cron-parse-expr
      expr
      fli-cron-expr
      err)
     (unless (fli:null-pointer-p (fli:dereference err))
       (error 'invalid-cron-expr :message
               (fli:convert-from-foreign-string (fli:dereference err))))
     (let ((ret (make-instance 'cron-expr
                                :fli-cron-expr fli-cron-expr)))

       (trivial-garbage:finalize ret
                                 (lambda ()
                                   (fli:free fli-cron-expr)))
       ret))))


;;;;;;;;;;;;;;;;;;;;;;
;; https://lisptips.com/post/11649360174/the-common-lisp-and-unix-epochs
(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))
;;;;;;;;;;;;;;;;;;;;;;;

(defmethod cron-next ((self cron-expr) &key (now (get-universal-time))
                                         (timezone 0))
  (let ((offset (* timezone 3600)))
    (- (unix-to-universal-time
        (%cron-next
         (fli-cron-expr self)
         (+ (universal-to-unix-time now) offset)))
       offset)))

(def-health-check verify-ccronexpr-is-loaded ()
  (cron-parse-expr "*/3 * * * * *"))
