(defpackage :util.test-better-easy-handler
  (:use :cl
        :fiveam
        :hex)
  (:import-from :hex
                :make-uri-regex
                :%make-uri-regex
                :better-easy-handler
                :url-handler-parse-tree
                :url-handler-request-args
                :split-url-parts
                :*url-list*)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:export))
(in-package :util.test-better-easy-handler)

(def-suite* :util.test-better-easy-handler)

(test split-url-parts
  (is (equal (list "foo" "/bar")
             (split-url-parts "/foo/bar")))
  (is (equal (list "foo" nil)
             (split-url-parts "/foo")))
  (is (equal (list "foo" "(/bar)")
             (split-url-parts "/foo(/bar)"))))

(test make-uri-regex
  (multiple-value-bind (regex vars) (make-uri-regex "/blog/:name")
    (is (equal (list "NAME") vars))
    (multiple-value-bind (res args) (scan-to-strings regex "/blog/foo-bar")
      (is-true res)
      (is (equalp #("foo-bar") args)))
    (multiple-value-bind (res args) (scan-to-strings regex "/blog/foo-bar/")
      (is-true res)
      (is (equalp #("foo-bar") args)))
    (multiple-value-bind (res args) (scan-to-strings regex "/blog")
      (is-false res)
      (is (equalp nil args)))
    (multiple-value-bind (res args) (scan-to-strings regex "/blog/")
      (is-false res)
      (is (equalp nil args)))))

(test without-any-args
  (multiple-value-bind (regex vars) (make-uri-regex "/blog")
    (is (equal nil vars))
    (multiple-value-bind  (res args) (scan-to-strings regex "/blog")
      (is-true res)
      (is (equalp #() args)))
    (multiple-value-bind  (res args) (scan-to-strings regex "/blog/")
      (is-true res)
      (is (equalp #() args)))))

(test optional-arg
  (multiple-value-bind (regex vars) (make-uri-regex "/blog(/:name)")
    (is (equal (list "NAME") vars))
    (multiple-value-bind (res args) (scan-to-strings regex "/blog/foo-bar")
      (is-true res)
      (is (equalp #("foo-bar")
                  args)))
    (multiple-value-bind (res args) (scan-to-strings regex "/blog/")
      (is-true res)
      (is (equalp #(nil) args)))
    (multiple-value-bind (res args) (scan-to-strings regex "/blog")
      (is-true res)
      (is (equalp #(nil) args)))))

(test long-name
  (multiple-value-bind (regex vars) (make-uri-regex "/assets/css/default.css")
    (is-true (scan-to-strings regex "/assets/css/default.css"))))

(test matches-root-url
  (multiple-value-bind (regex vars) (make-uri-regex "/")
    (is-true (scan-to-strings regex "/"))
    (is-false (scan-to-strings regex "/blah"))
    (is-false (scan-to-strings regex "blah/"))))

(test creates-proper-url-list
  (let ((*url-list* nil))
    (better-easy-handler (unused1 :uri "/foo(/:bar)") (car)
      nil)
    (is (= 1 (length *url-list*)))
    (is (eql 'unused1 (caar *url-list*)))
    (is (equal '(car) (url-handler-request-args (cdar *url-list*))))
    (is (equal '(:join (:path "foo") (:optional (:variable "bar"))) (url-handler-parse-tree (cdar *url-list*))))
    (is (equal "/foo/zoidberg" (hex:make-url 'unused1 :bar "zoidberg")))))

(test doesnt-match-subdirs
  (let ((regex (make-uri-regex "/foo/:bar")))
    (is-false (scan-to-strings regex "/foo/dfdfd/hello/world"))))
