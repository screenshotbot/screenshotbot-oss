;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/scim/test-filter
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/scim/model
                #:scim-user)
  (:import-from #:screenshotbot/scim/filter
                #:attr-path-sub-attr
                #:attr-path-name
                #:attr-path-uri
                #:invalid-filter
                #:parse-filter
                #:make-filter)
  (:import-from #:bknr.datastore
                #:store-object-id))
(in-package :screenshotbot/scim/test-filter)


(util/fiveam:def-suite)

(defun unparse-path (path)
  (format nil "~@[~a:~]~a~@[.~a~]"
          (attr-path-uri path)
          (attr-path-name path)
          (attr-path-sub-attr path)))

(defun ast (expr)
  "Parses EXPR, rendering the attr-paths as strings so that the result can
be compared with EQUAL."
  (labels ((walk (node)
             (ecase (first node)
               ((:or :and)
                (list (first node) (walk (second node)) (walk (third node))))
               (:not
                (list :not (walk (second node))))
               (:pr
                (list :pr (unparse-path (second node))))
               (:compare
                (list :compare (second node) (unparse-path (third node))
                      (fourth node)))
               (:value-path
                (list :value-path (unparse-path (second node))
                      (walk (third node)))))))
    (walk (parse-filter expr))))

(test attr-exp
  (is (equal '(:compare :eq "userName" "bjensen")
             (ast "userName eq \"bjensen\"")))
  (is (equal '(:compare :co "name.familyName" "O'Malley")
             (ast "name.familyName co \"O'Malley\"")))
  (is (equal '(:pr "title")
             (ast "title pr")))
  (is (equal '(:compare :gt "meta.lastModified" "2011-05-13T04:42:34Z")
             (ast "meta.lastModified gt \"2011-05-13T04:42:34Z\""))))

(test operators-and-keywords-are-case-insensitive
  (is (equal '(:compare :eq "userName" "bjensen")
             (ast "userName EQ \"bjensen\"")))
  (is (equal '(:pr "title")
             (ast "title PR")))
  (is (equal '(:and (:pr "title") (:pr "userType"))
             (ast "title pr AND userType pr")))
  (is (equal '(:not (:pr "title"))
             (ast "NOT (title pr)"))))

(test attr-path-with-schema-uri
  (is (equal '(:compare :sw
              "urn:ietf:params:scim:schemas:core:2.0:User:userName"
              "J")
             (ast "urn:ietf:params:scim:schemas:core:2.0:User:userName sw \"J\""))))

(test log-exp
  (is (equal '(:and (:pr "title") (:compare :eq "userType" "Employee"))
             (ast "title pr and userType eq \"Employee\"")))
  (is (equal '(:or (:pr "title") (:compare :eq "userType" "Intern"))
             (ast "title pr or userType eq \"Intern\""))))

(test and-binds-tighter-than-or
  (is (equal '(:or (:pr "a") (:and (:pr "b") (:pr "c")))
             (ast "a pr or b pr and c pr")))
  (is (equal '(:or (:and (:pr "a") (:pr "b")) (:pr "c"))
             (ast "a pr and b pr or c pr"))))

(test log-exp-is-left-associative
  (is (equal '(:and (:and (:pr "a") (:pr "b")) (:pr "c"))
             (ast "a pr and b pr and c pr")))
  (is (equal '(:or (:or (:pr "a") (:pr "b")) (:pr "c"))
             (ast "a pr or b pr or c pr"))))

(test grouping
  (is (equal '(:and (:or (:pr "a") (:pr "b")) (:pr "c"))
             (ast "(a pr or b pr) and c pr")))
  (is (equal '(:and (:compare :eq "userType" "Employee")
               (:or (:compare :co "emails" "example.com")
                (:compare :co "emails.value" "example.org")))
             (ast "userType eq \"Employee\" and (emails co \"example.com\" or emails.value co \"example.org\")"))))

(test not-exp
  (is (equal '(:not (:or (:pr "a") (:pr "b")))
             (ast "not (a pr or b pr)")))
  (is (equal '(:and (:compare :ne "userType" "Employee")
               (:not (:compare :co "emails" "example.com")))
             (ast "userType ne \"Employee\" and not (emails co \"example.com\")"))))

(test an-attribute-may-start-with-a-keyword
  (is (equal '(:pr "notes")
             (ast "notes pr")))
  (is (equal '(:and (:pr "a") (:pr "android"))
             (ast "a pr and android pr"))))

(test value-path
  (is (equal '(:value-path "emails" (:compare :eq "type" "work"))
             (ast "emails[type eq \"work\"]")))
  (is (equal '(:and (:compare :eq "userType" "Employee")
               (:value-path "emails"
                (:and (:compare :eq "type" "work")
                 (:compare :co "value" "@example.com"))))
             (ast "userType eq \"Employee\" and emails[type eq \"work\" and value co \"@example.com\"]")))
  (is (equal '(:or (:value-path "emails" (:compare :eq "type" "work"))
               (:value-path "ims" (:compare :eq "type" "xmpp")))
             (ast "emails[type eq \"work\"] or ims[type eq \"xmpp\"]"))))

(test value-path-may-not-be-nested
  (signals invalid-filter
    (parse-filter "emails[emails[type eq \"work\"]]")))

(test comp-values
  (is (equal '(:compare :eq "active" :true) (ast "active eq true")))
  (is (equal '(:compare :eq "active" :false) (ast "active eq false")))
  (is (equal '(:compare :eq "title" :null) (ast "title eq null")))
  (is (equal '(:compare :gt "count" 3) (ast "count gt 3")))
  (is (equal '(:compare :le "count" -3) (ast "count le -3")))
  (is (equal '(:compare :gt "count" 350.0d0) (ast "count gt 3.5e2")))
  (is (equal '(:compare :lt "count" 1.5d0) (ast "count lt 1.5"))))

(test comp-value-must-be-a-scalar
  (signals invalid-filter
    (parse-filter "a eq {\"b\": 1}"))
  (signals invalid-filter
    (parse-filter "a eq [1, 2]"))
  (signals invalid-filter
    (parse-filter "a eq tru"))
  (signals invalid-filter
    (parse-filter "a eq")))

(test string-escapes
  (is (equal '(:compare :eq "a" "say \"hi\"") (ast "a eq \"say \\\"hi\\\"\"")))
  (is (equal '(:compare :eq "a" "c:\\foo") (ast "a eq \"c:\\\\foo\"")))
  (is (equal (list :compare :eq "a" (format nil "x~ay" #\Newline))
             (ast "a eq \"x\\ny\"")))
  (is (equal '(:compare :eq "a" "A") (ast "a eq \"\\u0041\""))))

;;;; The rest of this section covers input that used to escape as
;;;; something other than INVALID-FILTER, and so would have been a 500.

(test surrogates-are-rejected
  ;; UTF-8 can't encode these, so they could never match a stored value.
  ;; Implementations disagree on CODE-CHAR here -- LispWorks returns NIL
  ;; (which used to reach us as a TYPE-ERROR), SBCL returns a character
  ;; -- so the filter has to rule them out itself.
  (signals invalid-filter
    (parse-filter "a eq \"\\ud800\""))
  (signals invalid-filter
    (parse-filter "a eq \"x\\udfffy\""))
  ;; CL-JSON doesn't combine a surrogate pair either
  (signals invalid-filter
    (parse-filter "a eq \"\\ud83d\\ude00\""))
  ;; ... but the character itself is fine written literally
  (is (equal (list :compare :eq "a" (string (code-char #x1F600)))
             (ast (format nil "a eq \"~a\"" (code-char #x1F600))))))

(test numbers-out-of-range-are-rejected
  ;; These used to quietly become double-float infinity, which then
  ;; compares as larger than every value we could be filtering on.
  (signals invalid-filter
    (parse-filter "count gt 1e999999"))
  (signals invalid-filter
    (parse-filter "count gt -1e999999"))
  (is (equal '(:compare :gt "count" 1.0d300)
             (ast "count gt 1e300"))))

(test absurdly-long-numbers-are-rejected
  (signals invalid-filter
    (parse-filter (format nil "count gt ~a"
                          (make-string 200000 :initial-element #\9)))))

(test numbers-follow-the-json-syntax
  ;; The JSON decoder stops at the end of the number, so a laxer grammar
  ;; here would read "007" as 0 and silently drop the rest.
  (signals invalid-filter
    (parse-filter "count eq 007"))
  (signals invalid-filter
    (parse-filter "count eq 1.2.3")))

(test deeply-nested-filters-are-rejected
  ;; The parser is recursive descent, so this would overflow the stack
  (flet ((nest (depth)
           (format nil "~aa pr~a"
                   (make-string depth :initial-element #\()
                   (make-string depth :initial-element #\)))))
    (is (equal '(:pr "a") (ast (nest 20))))
    (signals invalid-filter
      (parse-filter (nest 21)))
    (signals invalid-filter
      (parse-filter (nest 5000)))
    ;; ... but parens inside a string aren't nesting
    (is (equal (list :compare :eq "a" (make-string 100 :initial-element #\())
               (ast (format nil "a eq \"~a\""
                            (make-string 100 :initial-element #\()))))))

(test surrounding-whitespace-is-allowed
  (is (equal '(:pr "title") (ast "  title pr  "))))

(test invalid-filters
  (signals invalid-filter
    (parse-filter ""))
  (signals invalid-filter
    (parse-filter "userName eq"))
  (signals invalid-filter
    (parse-filter "userName foo \"bjensen\""))
  (signals invalid-filter
    (parse-filter "userName eq \"bjensen\" garbage"))
  (signals invalid-filter
    (parse-filter "1username eq \"bjensen\""))
  (signals invalid-filter
    (parse-filter "userName eq \"unterminated"))
  (signals invalid-filter
    (parse-filter "(userName pr")))

;;;; * Evaluating filters against a SCIM-USER

(def-fixture state ()
  (with-test-store ()
    (let* ((company (make-instance 'company))
           (user (make-instance 'scim-user
                                :company company
                                :user-name "bjensen@example.com"
                                :external-id "ext-0001"
                                :emails (list "barbara.jensen@example.com"
                                              "bjensen@example.org")))
           (empty-user (make-instance 'scim-user
                                      :company company
                                      :user-name "empty"
                                      :activep nil)))
      (&body))))

(defun matchesp (expr user)
  (funcall (make-filter expr) user))

(test eq-matches-case-insensitively
  (with-fixture state ()
    (is-true (matchesp "userName eq \"bjensen@example.com\"" user))
    (is-true (matchesp "userName eq \"BJensen@Example.com\"" user))
    (is-false (matchesp "userName eq \"someone-else\"" user))
    (is-true (matchesp "userName ne \"someone-else\"" user))
    (is-false (matchesp "userName ne \"bjensen@example.com\"" user))))

(test string-operators
  (with-fixture state ()
    (is-true (matchesp "userName co \"jensen\"" user))
    (is-false (matchesp "userName co \"nope\"" user))
    (is-true (matchesp "userName sw \"BJ\"" user))
    (is-false (matchesp "userName sw \"example\"" user))
    (is-true (matchesp "userName ew \".COM\"" user))
    (is-false (matchesp "userName ew \"bjensen\"" user))))

(test ordering-operators
  (with-fixture state ()
    (is-true (matchesp "userName gt \"a\"" user))
    (is-false (matchesp "userName gt \"z\"" user))
    (is-true (matchesp "userName lt \"z\"" user))
    (is-true (matchesp "userName ge \"bjensen@example.com\"" user))
    (is-false (matchesp "userName gt \"bjensen@example.com\"" user))
    (is-true (matchesp "userName le \"bjensen@example.com\"" user))))

(test presence
  (with-fixture state ()
    (is-true (matchesp "userName pr" user))
    (is-true (matchesp "emails pr" user))
    (is-false (matchesp "emails pr" empty-user))))

(test booleans-and-null
  (with-fixture state ()
    (is-true (matchesp "active eq true" user))
    (is-false (matchesp "active eq false" user))
    (is-true (matchesp "active eq false" empty-user))
    (is-true (matchesp "active eq null" empty-user))))

(test filter-on-id
  (with-fixture state ()
    (is-true (matchesp (format nil "id eq \"~a\"" (store-object-id user))
                       user))
    (is-false (matchesp (format nil "id eq \"~a\"" (store-object-id user))
                        empty-user))))

(test filter-on-external-id
  (with-fixture state ()
    (is-true (matchesp "externalId eq \"ext-0001\"" user))
    (is-false (matchesp "externalId eq \"ext-0002\"" user))
    (is-true (matchesp "externalId ne \"ext-0002\"" user))
    (is-true (matchesp "externalId sw \"ext-\"" user))
    (is-true (matchesp "externalId co \"0001\"" user))
    (is-true (matchesp "externalId pr" user))
    ;; This is how an IdP looks a user up after it has provisioned them
    (is-true (matchesp "userName pr and externalId eq \"ext-0001\"" user))))

(test external-id-attribute-name-is-case-insensitive
  (with-fixture state ()
    (is-true (matchesp "externalid eq \"ext-0001\"" user))
    (is-true (matchesp "EXTERNALID eq \"ext-0001\"" user))
    ;; ... and IdPs sometimes send the fully qualified attribute path
    (is-true (matchesp "urn:ietf:params:scim:schemas:core:2.0:User:externalId eq \"ext-0001\""
                       user))))

(test external-id-of-a-user-that-has-none
  (with-fixture state ()
    (is-false (matchesp "externalId pr" empty-user))
    (is-false (matchesp "externalId eq \"ext-0001\"" empty-user))
    (is-true (matchesp "externalId eq null" empty-user))))

(test external-id-slot-may-be-unbound
  ;; USER-TO-DTO reads this slot through IGNORE-ERRORS, so it can be
  ;; unbound on users that were stored before the slot existed. Filtering
  ;; on them has to be a miss, not an error.
  (with-fixture state ()
    (slot-makunbound user 'screenshotbot/scim/model::%external-id)
    (is-false (matchesp "externalId pr" user))
    (is-false (matchesp "externalId eq \"ext-0001\"" user))
    ;; the rest of the user is still filterable
    (is-true (matchesp "userName eq \"bjensen@example.com\"" user))))

(test multi-valued-attributes
  (with-fixture state ()
    (is-true (matchesp "emails eq \"bjensen@example.org\"" user))
    (is-true (matchesp "emails.value eq \"bjensen@example.org\"" user))
    (is-true (matchesp "emails co \"example.com\"" user))
    (is-false (matchesp "emails co \"example.net\"" user))
    (is-false (matchesp "emails eq \"bjensen@example.org\"" empty-user))))

(test value-paths-are-evaluated-per-value
  (with-fixture state ()
    (is-true (matchesp "emails[value ew \"example.org\"]" user))
    (is-false (matchesp "emails[value ew \"example.net\"]" user))
    ;; Both conditions have to hold for the *same* value
    (is-true (matchesp "emails[value sw \"bjensen\" and value ew \"example.org\"]"
                       user))
    (is-false (matchesp "emails[value sw \"barbara\" and value ew \"example.org\"]"
                        user))
    (is-false (matchesp "emails[value pr]" empty-user))))

(test logical-operators
  (with-fixture state ()
    (is-true (matchesp "userName pr and emails pr" user))
    (is-false (matchesp "userName pr and emails pr" empty-user))
    (is-true (matchesp "userName pr or emails pr" empty-user))
    (is-false (matchesp "not (userName pr)" user))
    (is-true (matchesp "not (emails pr)" empty-user))))

(test unsupported-attributes-are-an-invalid-filter
  (with-fixture state ()
    (signals invalid-filter
      (matchesp "nickName eq \"foo\"" user))
    (signals invalid-filter
      (matchesp "emails[nickName eq \"foo\"]" user))))

