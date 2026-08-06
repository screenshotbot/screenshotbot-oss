;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/scim/filter
  (:use #:cl)
  (:import-from #:esrap
                #:~ ;; a case insensitive terminal
                #:?
                #:defrule)
  (:import-from #:screenshotbot/scim/model
                #:scim-user-external-id
                #:scim-user-active-p
                #:scim-user-emails
                #:scim-user-user-name
                #:scim-user)
  (:export
   #:make-filter
   #:parse-filter
   #:attribute-values
   #:invalid-filter
   #:invalid-filter-message))
(in-package :screenshotbot/scim/filter)

;; FILTER    = attrExp / logExp / valuePath / *1"not" "(" FILTER ")"
;;
;;      valuePath = attrPath "[" valFilter "]"
;;                  ; FILTER uses sub-attributes of a parent attrPath
;;
;;      valFilter = attrExp / logExp / *1"not" "(" valFilter ")"
;;
;;      attrExp   = (attrPath SP "pr") /
;;                  (attrPath SP compareOp SP compValue)
;;
;;      logExp    = FILTER SP ("and" / "or") SP FILTER
;;
;;      compValue = false / null / true / number / string
;;                  ; rules from JSON (RFC 7159)
;;
;;      compareOp = "eq" / "ne" / "co" /
;;                         "sw" / "ew" /
;;                         "gt" / "lt" /
;;                         "ge" / "le"
;;
;;      attrPath  = [URI ":"] ATTRNAME *1subAttr
;;                  ; SCIM attribute name
;;                  ; URI is SCIM "schema" URI
;;
;;      ATTRNAME  = ALPHA *(nameChar)
;;
;;      nameChar  = "-" / "_" / DIGIT / ALPHA
;;
;;      subAttr   = "." ATTRNAME
;;                  ; a sub-attribute of a complex attribute

(define-condition invalid-filter (error)
  ((message :initarg :message
            :reader invalid-filter-message))
  (:report (lambda (self stream)
             (format stream "Invalid SCIM filter: ~a"
                     (invalid-filter-message self)))))

(defun invalid-filter (fmt &rest args)
  (error 'invalid-filter :message (apply #'format nil fmt args)))

(defstruct (attr-path (:constructor make-attr-path (uri name sub-attr)))
  "A parsed attrPath: an optional schema URI, the attribute name, and an
optional sub-attribute."
  uri
  name
  sub-attr)

(defconstant +max-number-length+ 64
  "The longest number we'll look at. Filtering on anything near this is
meaningless, and turning a few hundred thousand digits into a bignum
costs seconds.")

(defun surrogate-char-p (char)
  (<= #xD800 (char-code char) #xDFFF))

(defun number-char-p (char)
  (or (digit-char-p char)
      (member char '(#\- #\+ #\. #\e #\E))))

(defun number-run-length (text position end)
  "How many characters at POSITION could be part of a number. Zero for
anything else, in particular for a string, which is unlimited."
  (- (or (position-if-not #'number-char-p text :start position :end end)
         end)
     position))

(defun normalize-comp-value (value first-char)
  "CL-JSON decodes both false and null as NIL, so we tell them apart by
what was written, and keep the AST faithful to the filter."
  (cond
    ((eq value t) :true)
    ((null value) (if (char-equal first-char #\n) :null :false))
    (t value)))

(defun parse-comp-value (text position end)
  "An esrap terminal that reads one compValue.

RFC 7644 defines compValue in terms of JSON, so we hand the value to the
JSON decoder instead of reimplementing string escapes and number syntax
here -- and in particular instead of pointing READ-FROM-STRING, which is
far more powerful than we need, at untrusted input. CL-JSON unreads its
lookahead character, so the stream stops exactly at the end of the
value."
  (flet ((fail (reason)
           (values nil position reason)))
    (cond
      ((>= position end)
       (fail "expected a comparison value"))
      ((member (char text position) '(#\{ #\[))
       (fail "a comparison value can't be an object or an array"))
      ((> (number-run-length text position end) +max-number-length+)
       (fail "the number is too long"))
      (t
       (let ((stream (make-string-input-stream text position end)))
         (handler-case
             (let ((value (let ((*read-default-float-format* 'double-float))
                            (json:decode-json stream))))
               (cond
                 ((not (typep value '(or string real boolean)))
                  (fail "a comparison value must be false, null, true, a number or a string"))
                 ;; UTF-8 can't encode a surrogate, so a value containing
                 ;; one could never match anything we stored. We check
                 ;; for them ourselves because implementations disagree:
                 ;; CODE-CHAR returns NIL on LispWorks, so the decoder
                 ;; has already failed by this point, but SBCL hands
                 ;; back a character quite happily.
                 ((and (stringp value) (find-if #'surrogate-char-p value))
                  (fail "the string contains an unpaired surrogate"))
                 ;; An exponent out of double-float range overflows to
                 ;; infinity rather than signalling anything, and an
                 ;; infinity compares as larger than every value we
                 ;; could be filtering on.
                 ((and (floatp value)
                       (not (< most-negative-double-float
                               value
                               most-positive-double-float)))
                  (fail "the number is out of range"))
                 (t
                  (values (normalize-comp-value value (char text position))
                          (+ position (file-position stream))))))
           (error (e)
             ;; A bad escape, a truncated string, an unknown literal
             (fail (princ-to-string e)))))))))

(defun fold-logexp (op first rest)
  "Combines the productions of a left-associative chain of OP into an AST."
  (let ((result first))
    (dolist (item rest result)
      ;; Each item is (ws* keyword ws* operand)
      (setf result (list op result (fourth item))))))

(defun production-text (production)
  "Flattens an esrap production tree into the text it matched."
  (with-output-to-string (out)
    (labels ((walk (item)
               (typecase item
                 (null)
                 (character (write-char item out))
                 (string (write-string item out))
                 (cons (mapc #'walk item)))))
      (walk production))))


(defun check-attr-name (name text)
  (unless (and (plusp (length name))
               (alpha-char-p (char name 0))
               (every (lambda (char)
                        (or (alphanumericp char)
                            (member char '(#\- #\_))))
                      name))
    (invalid-filter "~s is not a valid attribute path" text)))

(defun parse-attr-path (text)
  "Splits TEXT into the optional schema URI, the attribute name, and the
optional sub-attribute."
  (let* ((colon (position #\: text :from-end t))
         (uri (when colon (subseq text 0 colon)))
         (rest (if colon (subseq text (1+ colon)) text))
         (dot (position #\. rest))
         (name (if dot (subseq rest 0 dot) rest))
         (sub-attr (when dot (subseq rest (1+ dot)))))
    (check-attr-name name text)
    (when sub-attr
      (check-attr-name sub-attr text))
    (make-attr-path uri name sub-attr)))

;;;; * The grammar
;;;;
;;;; The logExp rule in the RFC is left-recursive, so we rewrite it in
;;;; the usual way to give "or" a lower precedence than "and". The
;;;; parser produces an AST of the form:
;;;;
;;;;   (:or <node> <node>)
;;;;   (:and <node> <node>)
;;;;   (:not <node>)
;;;;   (:pr <attr-path>)
;;;;   (:compare <op> <attr-path> <value>)
;;;;   (:value-path <attr-path> <node>)

(defrule ws (+ (or #\Space #\Tab))
  (:constant nil))

(defrule ws* (* (or #\Space #\Tab))
  (:constant nil))

(defrule filter (and ws* or-expr ws*)
  (:function second))

(defrule or-expr (and and-expr (* (and ws* (~ "or") ws* and-expr)))
  (:destructure (first rest)
    (fold-logexp :or first rest)))

(defrule and-expr (and not-expr (* (and ws* (~ "and") ws* not-expr)))
  (:destructure (first rest)
    (fold-logexp :and first rest)))

(defrule not-expr (or not-group group value-path attr-exp))

(defrule not-group (and (~ "not") ws* #\( filter #\))
  (:destructure (not ws open expr close)
    (declare (ignore not ws open close))
    (list :not expr)))

(defrule group (and #\( filter #\))
  (:function second))

;;;; valFilter is FILTER without valuePath: a value path can't be
;;;; nested inside another value path.

(defrule val-filter (and ws* val-or-expr ws*)
  (:function second))

(defrule val-or-expr (and val-and-expr (* (and ws* (~ "or") ws* val-and-expr)))
  (:destructure (first rest)
    (fold-logexp :or first rest)))

(defrule val-and-expr (and val-not-expr (* (and ws* (~ "and") ws* val-not-expr)))
  (:destructure (first rest)
    (fold-logexp :and first rest)))

(defrule val-not-expr (or val-not-group val-group attr-exp))

(defrule val-not-group (and (~ "not") ws* #\( val-filter #\))
  (:destructure (not ws open expr close)
    (declare (ignore not ws open close))
    (list :not expr)))

(defrule val-group (and #\( val-filter #\))
  (:function second))

(defrule value-path (and attr-path #\[ val-filter #\])
  (:destructure (path open expr close)
    (declare (ignore open close))
    (list :value-path path expr)))

(defrule attr-exp (or pr-exp compare-exp))

(defrule pr-exp (and attr-path ws (~ "pr"))
  (:destructure (path ws pr)
    (declare (ignore ws pr))
    (list :pr path)))

(defrule compare-exp (and attr-path ws compare-op ws comp-value)
  (:destructure (path ws1 op ws2 value)
    (declare (ignore ws1 ws2))
    (list :compare op path value)))

(defrule compare-op (or (~ "eq") (~ "ne") (~ "co")
                        (~ "sw") (~ "ew")
                        (~ "gt") (~ "lt")
                        (~ "ge") (~ "le"))
  (:lambda (op)
    (intern (string-upcase op) :keyword)))

;;;; ATTRNAME, subAttr and the schema URI all use the same characters,
;;;; and the URI itself contains colons, so we read the whole token and
;;;; take it apart afterwards.

(defrule attr-path (and (alpha-char-p character) (* attr-path-char))
  (:lambda (production)
    (parse-attr-path (production-text production))))

(defrule attr-path-char (or (alphanumericp character) #\- #\_ #\. #\:))

(defrule comp-value (function parse-comp-value))

(defconstant +max-nesting-depth+ 20
  "How deeply groups may be nested. Real filters use two or three levels;
we're only here because the parser is recursive descent, and a couple of
hundred nested groups is enough to overflow the stack.")

(defun check-nesting-depth (expr)
  "Rejects a filter that nests groups too deeply, before we hand it to
the parser and blow the stack. Note that parens inside a string aren't
nesting, so we have to skip over strings as we count."
  (let ((depth 0)
        (in-string nil)
        (position 0)
        (end (length expr)))
    (loop while (< position end)
          do (let ((char (char expr position)))
               (incf position)
               (cond
                 (in-string
                  (case char
                    (#\\ (incf position))
                    (#\" (setf in-string nil))))
                 ((eql char #\") (setf in-string t))
                 ((member char '(#\( #\[))
                  (when (> (incf depth) +max-nesting-depth+)
                    (invalid-filter "Filter is nested more than ~a deep"
                                    +max-nesting-depth+)))
                 ((member char '(#\) #\]))
                  (decf depth)))))))

(defun parse-filter (expr)
  "Parses a SCIM filter expression into an AST. Signals INVALID-FILTER if
EXPR isn't a valid filter."
  (check-nesting-depth expr)
  (handler-case
      (esrap:parse 'filter expr)
    (esrap:esrap-parse-error (e)
      (invalid-filter "~a" e))))

;;;; * Evaluating a filter against an object

(defgeneric attribute-values (object name)
  (:documentation "Returns the list of values of the attribute NAME (a string, to be
compared case insensitively) on OBJECT. Single valued attributes return
a one element list, and multi-valued attributes return one element per
value. Signals INVALID-FILTER if OBJECT has no such attribute."))

(defmethod attribute-values ((self scim-user) name)
  (cond
    ((string-equal name "userName")
     (list (scim-user-user-name self)))
    ((string-equal name "id")
     (list (format nil "~a" (bknr.datastore:store-object-id self))))
    ((string-equal name "active")
     (list (scim-user-active-p self)))
    ((string-equal name "emails")
     (scim-user-emails self))
    ((string-equal name "externalId")
     ;; The slot is unbound on users stored before it existed, which
     ;; is why USER-TO-DTO reads it through IGNORE-ERRORS too
     (list (ignore-errors (scim-user-external-id self))))
    (t
     (invalid-filter "Unsupported attribute: ~a" name))))

(defmethod attribute-values ((self string) name)
  "Multi-valued attributes are stored as plain strings, so the only
sub-attribute we can answer is the canonical \"value\"."
  (cond
    ((string-equal name "value")
     (list self))
    ((or (string-equal name "type")
         (string-equal name "primary")
         (string-equal name "display"))
     nil)
    (t
     (invalid-filter "Unsupported attribute: ~a" name))))

(defun resolve-path (object path)
  "Returns the list of values that PATH refers to on OBJECT."
  ;; We ignore the schema URI: the attribute name lookup will complain
  ;; about anything we don't know how to filter on anyway.
  (let ((values (attribute-values object (attr-path-name path))))
    (cond
      ((attr-path-sub-attr path)
       (loop for value in values
             when value
               append (attribute-values value (attr-path-sub-attr path))))
      (t
       values))))

(defun make-filter (expr)
  "Returns a lambda, that takes one SCIM-USER object and returns true or false"
  (compile-node (parse-filter expr)))

(defun compile-node (node)
  (ecase (first node)
    (:or
     (let ((left (compile-node (second node)))
           (right (compile-node (third node))))
       (lambda (object)
         (or (funcall left object)
             (funcall right object)))))
    (:and
     (let ((left (compile-node (second node)))
           (right (compile-node (third node))))
       (lambda (object)
         (and (funcall left object)
              (funcall right object)))))
    (:not
     (let ((inner (compile-node (second node))))
       (lambda (object)
         (not (funcall inner object)))))
    (:pr
     (let ((path (second node)))
       (lambda (object)
         (loop for value in (resolve-path object path)
                 thereis (present-p value)))))
    (:compare
     (destructuring-bind (op path value) (rest node)
       (lambda (object)
         (loop for actual in (resolve-path object path)
                 thereis (scim-compare op actual value)))))
    (:value-path
     (destructuring-bind (path sub-filter) (rest node)
       (let ((inner (compile-node sub-filter)))
         (lambda (object)
           (loop for value in (resolve-path object path)
                   thereis (and value
                                (funcall inner value)))))))))

(defun present-p (value)
  (and value
       (not (equal value ""))))

(defun scim-compare (op actual expected)
  (ecase op
    (:eq (scim-equal actual expected))
    (:ne (not (scim-equal actual expected)))
    (:co (and (stringp actual) (stringp expected)
              (search expected actual :test #'char-equal)
              t))
    (:sw (and (stringp actual) (stringp expected)
              (str:starts-with-p expected actual :ignore-case t)))
    (:ew (and (stringp actual) (stringp expected)
              (str:ends-with-p expected actual :ignore-case t)))
    ((:gt :lt :ge :le)
     (scim-order op actual expected))))

(defun scim-equal (actual expected)
  (typecase expected
    (string (and (stringp actual)
                 (string-equal actual expected)))
    (real (and (realp actual)
               (= actual expected)))
    (t
     (ecase expected
       (:true (not (null actual)))
       ;; A missing value reads as both false and null, which is the
       ;; best we can do without a schema.
       ((:false :null) (null actual))))))

(defun scim-order (op actual expected)
  (multiple-value-bind (lessp equalp)
      (cond
        ((and (stringp actual) (stringp expected))
         (values (and (string-lessp actual expected) t)
                 (and (string-equal actual expected) t)))
        ((and (realp actual) (realp expected))
         (values (< actual expected)
                 (= actual expected)))
        (t
         (return-from scim-order nil)))
    (ecase op
      (:lt lessp)
      (:le (or lessp equalp))
      (:ge (not lessp))
      (:gt (and (not lessp) (not equalp))))))
