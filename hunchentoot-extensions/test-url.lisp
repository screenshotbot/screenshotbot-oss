(defpackage :util.test-url
  (:use :cl
        :fiveam
        :util)
  (:import-from :hex
                :missing-required-arg))
(in-package :util.test-url)

(def-suite* :util.test-url)

(test simple-make-url
  (is (equal "/foo/bar" (make-url "/foo/bar"))))

(test simple-param
  (is (equal "/foo/bar?key=val" (make-url "/foo/bar" :key "val")))
  (is (equal "/foo/bar?key=val&boo=eey" (make-url "/foo/bar" :key "val" :boo "eey"))))

(test there's-already-a-?-mark
  (is (equal "/foo/bar?key=val&boo=eey" (make-url "/foo/bar?key=val" :boo "eey"))))

(test what-if-arg-is-hash-map
  (is (equal "/foo/bar?add{line1}=foo"
             (make-url "/foo/bar" :add (alexandria:plist-hash-table `("line1" "foo"))))))

(test what-if-arg-is-hash-map-with-multiple-value
  (let ((result (make-url "/foo/bar" :add (alexandria:plist-hash-table `("line1" "foo" "line2" "bar")))))
    (is (or
         (equal "/foo/bar?add{line1}=foo&add{line2}=bar"
                result)
         (equal "/foo/bar?add{line2}=bar&add{line1}=foo" result)))))

(test url-with-arg
  (is (equal "/foo/zoidberg" (make-url "/foo/:bar" :bar "zoidberg")))
  (is (equal "/foo/zoidberg?car=2" (make-url "/foo/:bar" :bar "zoidberg" :car 2)))
  (signals missing-required-arg
    (make-url "/foo/:bar")))

(test variable-right-at-top
  (is (equal "/zoidberg" (make-url "/:bar" :bar "zoidberg")))
  (is (equal "/" (make-url "/"))))

(test url-with-optional-arg
  (is (equal "/foo/page/zoidberg" (make-url "/foo(/page/:bar)" :bar "zoidberg")))
  (is (equal "/foo/page/zoidberg?car=2" (make-url "/foo(/page/:bar)" :bar "zoidberg" :car 2)))
  (is (equal "/foo" (make-url "/foo(/page/:bar)"))))

(test make-url-with-/
  (is (equal "/" (make-url "/"))))

(test use-hash-in-name
  (is (equal "/foo/zoid%23berg" (make-url "/foo/:bar" :bar "zoid#berg"))))

(test use-integer-in-name
  (is (equal "/foo/23" (make-url "/foo/:bar" :bar 23))))
