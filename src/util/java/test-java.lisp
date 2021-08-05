;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package util/java/test-java
  (:use #:cl
        #:fiveam)
  (:import-from ./java
                #:*type-locator*
                #+ccl
                #:fix-arg-for-jmethod-invoke
                #:safe-make-java-array
                #:java-objects-eq
                #:invoke
                #:define-java-callers
                #:jvref
                #:java-array-p
                #:list->array
                #:java-equals
                #:new-instance
                #:find-java-class
                #:find_class2
                #:jclass-of
                #:primitive-locator
                #:make-type-locator))

(defvar *jippo-locator*
  (make-type-locator
   :package "com.tdrhq"
   :imports '("java.util.*"
              "java.io.*"
              "java.lang.reflect.*"
              "com.tdrhq.*"
              "com.tdrhq.core.*"
              "com.tdrhq2.*"
              "io.jipr.eyepatch.iface.*"
              "org.apache.commons.lang3.reflect.*")))

(defmacro assert-jeq (x y)
  `(is (java-objects-eq ,x ,y)))

(defmacro assert-jequal (x y)
  `(is (java-equals ,x ,y)))

(defvar *java-lang-locator* (make-type-locator :package "" :imports nil :class "FooBar"))

(test simple-location
  (let ((*type-locator* *java-lang-locator*))
    #+ccl
    (is (eq 'cl+j::java-class (type-of (jclass-of '|String|))))
    (is (java-objects-eq (find_class2 "java.lang.String") (jclass-of '|String|)))
    (is (java-objects-eq (find_class2 "java.lang.Long") (jclass-of '|Long|)))))

(test without-locator
  (is (java-objects-eq (find_class2 "java.lang.String") (jclass-of '|java.lang.String|))))

(test with-package
  (let ((*type-locator* (make-type-locator :package "com.tdrhq")))
    (is (java-objects-eq (find_class2 "com.tdrhq.SimpleNativeLibrary") (jclass-of '|SimpleNativeLibrary|)))
    (is (java-objects-eq (find_class2 "java.lang.String") (jclass-of '|String|)))))

(test with-imports
  (let ((*type-locator* (make-type-locator
                         :package "com.tdrhq"
                         :imports '("java.io.File" "java.io.IOException"))))
    (is (java-objects-eq (find_class2 "com.tdrhq.SimpleNativeLibrary") (jclass-of '|SimpleNativeLibrary|)))
    (is (java-objects-eq (find_class2 "java.lang.String") (jclass-of '|String|)))
    (is (java-objects-eq (find_class2 "java.io.IOException") (jclass-of '|IOException|)))
    (is (java-objects-eq (find_class2 "java.io.File") (jclass-of '|File|)))))

(test with-wildcards
  (let ((*type-locator* (make-type-locator
                         :package "com.tdrhq"
                         :imports '("java.io.*"))))
    (assert-jeq (find_class2 "com.tdrhq.SimpleNativeLibrary") (jclass-of '|SimpleNativeLibrary|))
    (assert-jeq (find_class2 "java.lang.String") (jclass-of '|String|))
    (assert-jeq (find_class2 "java.io.IOException") (jclass-of '|IOException|))
    (assert-jeq (find_class2 "java.io.File") (jclass-of '|File|))
    (assert-jeq (find_class2 "int") (jclass-of :int))))

(test new-instance
  (let ((*type-locator* *jippo-locator*))
    (format t "trying String~%")
    (is (equal "" (new-instance '|String|)))
    (format t "trying String foo~%")
    (is (equal "foo" (new-instance '|String| "foo")))
    (format t "trying Integer 20~%")
    (is (equal 20 (new-instance '|Integer| "20")))))

(test invoke
  (let ((*type-locator* *jippo-locator*))
    (is (equal 6 (invoke "foobar" '|length|)))
    (is (equal 4 (invoke "foobar" '|indexOf| "a")))
    (is (equal "4" (invoke '|String| '|valueOf| 4)))))

(test use-keywords
  (let ((*type-locator* *jippo-locator*))
    (is (equal 6 (invoke "foobar" :|length|)))
    (is (equal 4 (invoke "foobar" "indexOf" "a")))
    (is (equal "4" (invoke :|String| "valueOf" 4)))))


;; todo: finish before landing
(test read-field
  (let ((*type-locator* *jippo-locator*))
    (is (equal 64 (util/java:read-java-field '|java.lang.Long| '|SIZE|)))
    (assert-jequal (new-instance '|java.lang.Boolean| "true") (util/java:read-java-field '|java.lang.Boolean| '|TRUE|))))

(test make-hash-table
  (let ((ht (make-hash-table)))
    (setf (gethash 'foo ht) 'bar)
    (is (eq 'bar (gethash 'foo ht)))
    (is (eq nil (gethash 'car ht)))))


#+lispworks
(define-java-callers "java.lang.Integer"
  (int-value "valueOf" :signatures ("int")))

#+ccl
(defun int-value (x)
  (cl+j:jmethod "java.lang.Integer" "valueOf" (cl+j:jprim-int x)))

(test make-java-array
  (let ((arr (safe-make-java-array '|java.lang.String| 10)))
    (setf (jvref arr 0) "foo")
    (is (equal "foo" (jvref arr 0))))

  (let ((arr (list->array '|java.lang.Integer| (mapcar 'int-value '(1 2 3)))))
    ;; there's a small behaviour difference that I'm too lazy to
    ;; fix. Hopefully this doesn't affect anything in screenshotbot
    ;; itself. It's rare that I call jvref directly. This was mostly
    ;; for JIPR.
    #+ccl
    (progn
      (is (eql 1 (jvref arr 0)))
      (is (eql 2 (jvref arr 1)))
      (is (eql 3 (jvref arr 2))))
    #+lispworks
    (progn
      (assert-jequal (int-value 1) (jvref arr 0))
      (assert-jequal (int-value 2) (jvref arr 1))
      (assert-jequal (int-value 3) (jvref arr 2)))))

(test list->array
  (log:debug "class is ~a~%"
          (invoke
           (invoke (list->array '|java.lang.Integer| (mapcar 'int-value '(1 2 3))) '|getClass|)
           '|isArray|))
  (is-true (java-array-p (list->array '|java.lang.Integer| (mapcar 'int-value '(1 2 3)))))
  (is-false (java-array-p '(1 2 3))))

(test primitive-lookup
  (is (java-objects-eq (find-java-class :int) (primitive-locator :int))))

#+ccl
(test fix-arg-for-jmethod-invoek
  (is (not (stringp (fix-arg-for-jmethod-invoke "foo")))))
