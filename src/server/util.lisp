;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :server/util
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro))
(in-package :server/util)

#+lispworks
(def-easy-macro with-error-handlers (&fn fn)
  (let ((counter 0))
   (handler-bind ((error
                    (lambda (e)
                      (when (and
                             ;; Could we loop forever?
                             (< counter 5)
                             (loop for (type substr) in
                                                     (list
                                                      (list 'simple-error "is not congruent with")
                                                      (list 'conditions:simple-program-error "is defined as an ordinary function"))
                                   if (and
                                       (typep e type)
                                       (str:containsp substr (format nil "~a" e)))
                                     return t))
                        (incf counter)
                        ;; Usually this means "modify the lambda list and delete existing methods"
                        ;; For is defined as an ordinary function, this would be "Discard existing definition and create generic function"
                        (invoke-restart 'continue)))))
     (fn))))

#+(and lispworks linux)
(defun safe-load-system (name &key fix-by-default)
  (bt:with-lock-held ((symbol-value (read-from-string "bknr.cluster/server::*commit-lock*")))
    (cond
      (fix-by-default
       (with-error-handlers ()
         (asdf:load-system name)))
      (t
       (asdf:load-system name)))))
