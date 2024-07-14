;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util.test-mquery
  (:use :cl
        :fiveam
        :mquery
        :markup)
  (:import-from :mquery
                :split-query-components))
(in-package :util.test-mquery)

(def-suite* :util.test-mquery)

(named-readtables:in-readtable markup:syntax)

(def-fixture state ()
  (let* ((inner-div <div class= "trouble" name= "zoid" ></div>)
         (complex-body <html><h1 name= "foo" >hello</h1>
                      ,(progn inner-div)</html>))
    (with-document (complex-body)
     (&body))))

(test simple-query
  (with-fixture state ()
    (is-true ($ "h1"))
    (is (equal :h1 (xml-tag-name (car  ($ "h1")))))
    (is (equal "foo" (attr ($ "h1")  "name")))
    (is (equal "foo" (attr ($ "h1")  :name)))))

(test set-attr
  (with-fixture state ()
    (setf (attr ($ "h1") "name") "bar")
    (is (equal "bar" (attr ($ "h1") "name")))))

(test find-div
  (with-fixture state ()
    (is (eql inner-div (car ($ "div"))))))

(test find-by-classname
  (with-fixture state ()
    (is (eql inner-div (car ($ ".trouble"))))))

(test find-by-classname-and-tag
  (with-fixture state ()
    (is (eql inner-div (car ($ "div.trouble"))))
    (is (eql inner-div (car ($ "div.trouble.trouble"))))))

(test add-class
  (with-fixture state ()
    (add-class ($ "div") "maker")
    (is (equal "trouble maker"
               (attr ($ "div") "class")))))

(test remove-class
  (with-fixture state ()
    (remove-class ($ "div") "trouble")
    (is (equal "" (attr ($ "div") "class")))))

(test name
  (with-fixture state ()
    (is (eql inner-div
             (car ($ (namequery "zoid")))))))

(test name-by-query
  (with-fixture state ()
    (is (eql inner-div
             (car ($ "div[name='zoid']"))))))

(test parent
  (with-fixture state ()
    (is (eql complex-body
             (parent ($ "div"))))))

(test after
  (with-fixture state ()
    (is (eql inner-div (after ($ "h1"))))))

(test insert-after
  (with-fixture state ()
    (let ((new-tag <div></div>))
      (setf (after ($ "h1"))  new-tag)
      (is (eql new-tag (after ($ "h1")))))))

(test text
  (let ((newtag <h1>hello</h1>))
    (is (equal "hello" (text newtag))))
  (let ((newtag <html><h1>hello</h1></html>))
    (is (equal "hello" (text ($ "h1" newtag)))))
  (let ((newtag <html><h1>hello <b>world</b></h1></html>))
    (is (equal "hello world" (text ($ "h1" newtag))))))

(test split-query-components
  (is (equal (list "foo" nil)
             (split-query-components "foo")))
  (is (equal (list ".foo" nil)
             (split-query-components ".foo")))
  (is (equal (list "foo" ".bar")
             (split-query-components "foo.bar")))
  (is (equal (list "foo" ".bar.car")
             (split-query-components "foo.bar.car")))
  (is (equal (list "foo" "[attr='val']")
             (split-query-components "foo[attr='val']"))))

(test finds-under-merge-tags-too
  (let ((doc <div>
  <markup:merge-tag>
    <div class= "foobar" />
  </markup:merge-tag>
             </div>))
    (with-document (doc)
      (is (eql 0 (length (mquery:$ ".car"))))
      (is (eql 1 (length (mquery:$ ".foobar")))))))
