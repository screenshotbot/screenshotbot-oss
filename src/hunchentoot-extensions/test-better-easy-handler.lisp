(defpackage :hunchentoot-extensions/test-better-easy-handler
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
  (:import-from #:hunchentoot-extensions
                #:def-named-url
                #:%only-request-of-type)
  (:import-from #:util/testing
                #:with-fake-request)
  (:export))
(in-package :hunchentoot-extensions/test-better-easy-handler)

(util/fiveam:def-suite)

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

(def-named-url foo "/bar/car/dar/foo")

(test named-url-gets-mapped
  (is (equal "/bar/car/dar/foo"
             (hex:make-url 'foo))))

(test redirect-signal
  (signals hex:redirected
    (hex:safe-redirect "/")))

(defun accepts-method-p (method)
  "Can a handler even be registered for METHOD?

This is the whole surface of the :OPTIONS change -- the assertion in
%ONLY-REQUEST-OF-TYPE is what decides, and it fires when the matcher is
built rather than when a request arrives."
  (and (%only-request-of-type "/foo" method) t))

(defun serves-get-p (method)
  "Would a handler registered for METHOD serve the GET that
WITH-FAKE-REQUEST sets up?"
  (with-fake-request (:script-name "/foo")
    (and (funcall (%only-request-of-type "/foo" method) hunchentoot:*request*)
         t)))

(test options-handlers-can-be-registered
  "CORS preflight arrives as OPTIONS, so a handler has to be able to ask
for that method. Before this, :OPTIONS tripped the assertion."
  (is-true (accepts-method-p :options)))

(test the-previously-allowed-methods-are-still-allowed
  (dolist (method '(nil :get :post :delete :put :patch))
    (is-true (accepts-method-p method)
             "expected ~s to still be registerable" method)))

(test an-unknown-method-is-still-refused
  "The assertion is what stops a typo becoming a handler that never fires,
so widening it must not turn it off."
  (signals error (accepts-method-p :head))
  (signals error (accepts-method-p :trace))
  (signals error (accepts-method-p :post-ish)))

(test dispatch-is-unchanged-for-existing-methods
  (is-true (serves-get-p :get))
  (is-false (serves-get-p :post))
  ;; NIL still means any method.
  (is-true (serves-get-p nil))
  ;; And an OPTIONS handler does not start swallowing GETs.
  (is-false (serves-get-p :options)))
