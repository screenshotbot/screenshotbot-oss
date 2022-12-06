;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/test-migrations
  (:use #:cl
        #:fiveam)
  (:import-from #:util/migrations
                #:*moved-syms*
                #:symbol-in-both-packages
                #:ensure-symbol-in-package)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/test-migrations)


(util/fiveam:def-suite)

(def-fixture state ()
  (let ((*moved-syms* nil))
   (let ((package (make-package :%dum1))
         (package2 (make-package :%dum2)))
     (unwind-protect
          (&body)
       (delete-package package)
       (delete-package package2)))))

(defun ensure-foo ()
  (is-false (find-symbol "FOO" :%dum1))
  (is-true (find-symbol "FOO" :%dum2)))

(test simple-migration-when-symbol-does-not-exist
  "This is the situation of restarting an image with this migration."

  (with-fixture state ()
    (ensure-symbol-in-package
     #:foo
     :old #:%dum1
     :new #:%dum2)
    (is-true (uiop:find-symbol* :foo :%dum2))

    (ensure-foo)

    (is (eql
         (find-package :%dum2)
         (symbol-package (uiop:find-symbol* :foo :%dum2))))))


(test simple-migration-when-symbol-is-in-old-package
  (with-fixture state ()
    (intern "FOO" :%dum1)
    (ensure-symbol-in-package
     #:foo
     :old #:%dum1
     :new #:%dum2)

    (is (eql
         (find-package :%dum2)
         (symbol-package (uiop:find-symbol* :foo :%dum2))))))

(test simple-migration-when-symbol-is-in-new-package
  (with-fixture state ()
    (intern "FOO" :%dum2)
    (ensure-symbol-in-package
     #:foo
     :old #:%dum1
     :new #:%dum2)

    (ensure-foo)
    (is-true (find-symbol "FOO" :%dum2))
    (is-false (find-symbol "FOO" :%dum1))

    (is (eql
         (find-package :%dum2)
         (symbol-package (uiop:find-symbol* :foo :%dum2))))))


(test simple-migration-when-symbol-is-in-both-packages
  (with-fixture state ()
    (intern "FOO" :%dum2)
    (intern "FOO" :%dum1)
    (signals symbol-in-both-packages
     (ensure-symbol-in-package
      #:foo
      :old #:%dum1
      :new #:%dum2))))

(test the-same-symbol-is-in-both-packages
  (with-fixture state ()
    (let ((sym (intern "FOO" :%dum2)))
      (import sym :%dum1))

    (ensure-symbol-in-package
     #:foo
     :old #:%dum1
     :new #:%dum2)

    (ensure-foo)))
