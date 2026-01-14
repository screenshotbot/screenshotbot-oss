;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/test-css-tokenizer
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/replay/css-tokenizer
                #:token-value
                #:url-token
                #:bad-url-token
                #:string-token
                #:bad-string-token
                #:function-token
                #:consume-url-token
                #:consume-string-token
                #:consume-ident-like-token
                #:consume-ident-sequence
                #:whitespacep
                #:non-printable-p
                #:newline-p
                #:hex-digit-p
                #:letter-p
                #:non-ascii-p
                #:ident-start-p
                #:digit-p
                #:ident-code-point-p)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that))
(in-package :screenshotbot/replay/test-css-tokenizer)


(util/fiveam:def-suite)


(test whitespacep
  (is-false (whitespacep #\a))
  (is-true (whitespacep #\Return)))


(test non-printable-p
  (is-true (non-printable-p (code-char 0)))
  (is-true (non-printable-p (code-char 8)))
  (is-true (non-printable-p (code-char 11)))
  (is-true (non-printable-p (code-char 31)))
  (is-true (non-printable-p (code-char 127)))
  (is-false (non-printable-p #\a))
  (is-false (non-printable-p #\Space)))

(test newline-p
  (is-true (newline-p #\Newline))
  (is-true (newline-p #\Return))
  (is-true (newline-p #\Page))
  (is-false (newline-p #\Space))
  (is-false (newline-p #\a)))

(test hex-digit-p
  (is-true (hex-digit-p #\0))
  (is-true (hex-digit-p #\9))
  (is-true (hex-digit-p #\A))
  (is-true (hex-digit-p #\F))
  (is-true (hex-digit-p #\a))
  (is-true (hex-digit-p #\f))
  (is-false (hex-digit-p #\G))
  (is-false (hex-digit-p #\g))
  (is-false (hex-digit-p #\Space)))

(test consume-url-token-basic
  "Test basic URL consumption"
  (let ((token (consume-url-token
                (make-string-input-stream "https://google.com)"))))
    (assert-that token (has-typep 'url-token))
    (is (string= "https://google.com" (token-value token)))))

(test consume-url-token-with-leading-whitespace
  "Test URL with leading whitespace"
  (let ((token (consume-url-token
                (make-string-input-stream "  https://example.com)"))))
    (assert-that token (has-typep 'url-token))
    (is (string= "https://example.com" (token-value token)))))

(test consume-url-token-with-trailing-whitespace
  "Test URL with trailing whitespace before closing paren"
  (let ((token (consume-url-token
                (make-string-input-stream "https://example.com  )"))))
    (assert-that token (has-typep 'url-token))
    (is (string= "https://example.com" (token-value token)))))

(test consume-url-token-eof
  "Test URL without closing paren (EOF)"
  (let ((token (consume-url-token
                (make-string-input-stream "https://example.com"))))
    (assert-that token (has-typep 'url-token))
    (is (string= "https://example.com" (token-value token)))))

(test consume-url-token-with-escaped-char
  "Test URL with escaped character"
  (let ((token (consume-url-token
                (make-string-input-stream "https://example.com/\\(test\\))"))))
    (assert-that token (has-typep 'url-token))
    (is (string= "https://example.com/(test)" (token-value token)))))

(test consume-url-token-with-hex-escape
  "Test URL with hex escape sequence"
  (let ((token (consume-url-token
                (make-string-input-stream "https://example.com/\\20test)"))))
    (assert-that token (has-typep 'url-token))
    (is (string= "https://example.com/ test" (token-value token)))))

(test consume-url-token-bad-url-quote
  "Test bad URL with quote character"
  (let ((token (consume-url-token
                (make-string-input-stream "https://\"bad.com)"))))
    (assert-that token (has-typep 'bad-url-token))))

(test consume-url-token-bad-url-single-quote
  "Test bad URL with single quote character"
  (let ((token (consume-url-token
                (make-string-input-stream "https://'bad.com)"))))
    (assert-that token (has-typep 'bad-url-token))))

(test consume-url-token-bad-url-paren
  "Test bad URL with unescaped left parenthesis"
  (let ((token (consume-url-token
                (make-string-input-stream "https://(bad.com)"))))
    (assert-that token (has-typep 'bad-url-token))))

(test consume-url-token-bad-url-non-printable
  "Test bad URL with non-printable character"
  (let ((token (consume-url-token
                (make-string-input-stream
                 (format nil "https://bad~Ccom)" (code-char 0))))))
    (assert-that token (has-typep 'bad-url-token))))

(test consume-url-token-bad-url-invalid-escape
  "Test bad URL with invalid escape (backslash before newline)"
  (let ((token (consume-url-token
                (make-string-input-stream
                 (format nil "https://bad\\~Acom)" #\Newline)))))
    (assert-that token (has-typep 'bad-url-token))))

(test consume-url-token-bad-url-whitespace-not-followed-by-paren
  "Test bad URL with whitespace not followed by closing paren"
  (let ((token (consume-url-token
                (make-string-input-stream "https://bad.com more)"))))
    (assert-that token (has-typep 'bad-url-token))))

(test consume-url-token-empty-url
  "Test empty URL"
  (let ((token (consume-url-token
                (make-string-input-stream ")"))))
    (assert-that token (has-typep 'url-token))
    (is (string= "" (token-value token)))))

(test consume-url-token-with-path
  "Test URL with path and query"
  (let ((token (consume-url-token
                (make-string-input-stream "https://example.com/path?query=value)"))))
    (assert-that token (has-typep 'url-token))
    (is (string= "https://example.com/path?query=value" (token-value token)))))

(test consume-url-token-with-fragment
  "Test URL with fragment"
  (let ((token (consume-url-token
                (make-string-input-stream "https://example.com#fragment)"))))
    (assert-that token (has-typep 'url-token))
    (is (string= "https://example.com#fragment" (token-value token)))))

;;; consume-string-token tests

(test consume-string-token-basic-double-quote
  "Test basic string with double quotes"
  (let ((token (consume-string-token
                (make-string-input-stream "hello world\"")
                #\")))
    (assert-that token (has-typep 'string-token))
    (is (string= "hello world" (token-value token)))))

(test consume-string-token-basic-single-quote
  "Test basic string with single quotes"
  (let ((token (consume-string-token
                (make-string-input-stream "hello world'")
                #\')))
    (assert-that token (has-typep 'string-token))
    (is (string= "hello world" (token-value token)))))

(test consume-string-token-empty
  "Test empty string"
  (let ((token (consume-string-token
                (make-string-input-stream "\"")
                #\")))
    (assert-that token (has-typep 'string-token))
    (is (string= "" (token-value token)))))

(test consume-string-token-eof
  "Test string ending with EOF (parse error but returns token)"
  (let ((token (consume-string-token
                (make-string-input-stream "hello world")
                #\")))
    (assert-that token (has-typep 'string-token))
    (is (string= "hello world" (token-value token)))))

(test consume-string-token-with-escaped-quote
  "Test string with escaped quote"
  (let ((token (consume-string-token
                (make-string-input-stream "hello \\\"world\\\"\"")
                #\")))
    (assert-that token (has-typep 'string-token))
    (is (string= "hello \"world\"" (token-value token)))))

(test consume-string-token-with-escaped-single-quote
  "Test string with escaped single quote"
  (let ((token (consume-string-token
                (make-string-input-stream "hello \\'world\\'\"")
                #\")))
    (assert-that token (has-typep 'string-token))
    (is (string= "hello 'world'" (token-value token)))))

(test consume-string-token-with-hex-escape
  "Test string with hex escape sequence"
  (let ((token (consume-string-token
                (make-string-input-stream "hello\\20world\"")
                #\")))
    (assert-that token (has-typep 'string-token))
    (is (string= "hello world" (token-value token)))))

(test consume-string-token-with-escaped-backslash
  "Test string with escaped backslash"
  (let ((token (consume-string-token
                (make-string-input-stream "hello\\\\world\"")
                #\")))
    (assert-that token (has-typep 'string-token))
    (is (string= "hello\\world" (token-value token)))))

(test consume-string-token-with-escaped-newline
  "Test string with escaped newline (backslash followed by newline)"
  (let ((token (consume-string-token
                (make-string-input-stream
                 (format nil "hello\\~Aworld\"" #\Newline))
                #\")))
    (assert-that token (has-typep 'string-token))
    (is (string= "helloworld" (token-value token)))))

(test consume-string-token-bad-string-unescaped-newline
  "Test bad string with unescaped newline"
  (let ((token (consume-string-token
                (make-string-input-stream
                 (format nil "hello~Aworld\"" #\Newline))
                #\")))
    (assert-that token (has-typep 'bad-string-token))))

(test consume-string-token-bad-string-unescaped-return
  "Test bad string with unescaped carriage return"
  (let ((token (consume-string-token
                (make-string-input-stream
                 (format nil "hello~Aworld\"" #\Return))
                #\")))
    (assert-that token (has-typep 'bad-string-token))))

(test consume-string-token-bad-string-unescaped-page
  "Test bad string with unescaped form feed"
  (let ((token (consume-string-token
                (make-string-input-stream
                 (format nil "hello~Aworld\"" #\Page))
                #\")))
    (assert-that token (has-typep 'bad-string-token))))

(test consume-string-token-with-multiple-escapes
  "Test string with multiple escape sequences"
  (let ((token (consume-string-token
                (make-string-input-stream "\\22hello\\20world\\22\"")
                #\")))
    (assert-that token (has-typep 'string-token))
    (is (string= "\"hello world\"" (token-value token)))))

(test consume-string-token-backslash-at-eof
  "Test string ending with backslash at EOF"
  (let ((token (consume-string-token
                (make-string-input-stream "hello\\")
                #\")))
    (assert-that token (has-typep 'string-token))
    (is (string= "hello" (token-value token)))))

(test consume-string-token-with-special-chars
  "Test string with special characters"
  (let ((token (consume-string-token
                (make-string-input-stream "hello!@#$%^&*()_+-={}[]|:;<>?,./\"")
                #\")))
    (assert-that token (has-typep 'string-token))
    (is (string= "hello!@#$%^&*()_+-={}[]|:;<>?,./" (token-value token)))))

(test consume-string-token-unicode-escape
  "Test string with unicode escape"
  (let ((token (consume-string-token
                (make-string-input-stream "\\1F600\"")
                #\")))
    (assert-that token (has-typep 'string-token))
    ;; U+1F600 is the grinning face emoji
    (is (string= (string (code-char #x1F600)) (token-value token)))))

;;; Identifier helper function tests

(test letter-p-test
  (is-true (letter-p #\a))
  (is-true (letter-p #\Z))
  (is-true (letter-p #\m))
  (is-false (letter-p #\0))
  (is-false (letter-p #\_))
  (is-false (letter-p #\-)))

(test digit-p-test
  (is-true (digit-p #\0))
  (is-true (digit-p #\9))
  (is-true (digit-p #\5))
  (is-false (digit-p #\a))
  (is-false (digit-p #\A)))

(test non-ascii-p-test
  (is-true (non-ascii-p (code-char #x80)))
  (is-true (non-ascii-p (code-char #x1F600)))
  (is-false (non-ascii-p #\a))
  (is-false (non-ascii-p #\0)))

(test ident-start-p-test
  (is-true (ident-start-p #\a))
  (is-true (ident-start-p #\Z))
  (is-true (ident-start-p #\_))
  (is-true (ident-start-p (code-char #x80)))
  (is-false (ident-start-p #\0))
  (is-false (ident-start-p #\-)))

(test ident-code-point-p-test
  (is-true (ident-code-point-p #\a))
  (is-true (ident-code-point-p #\Z))
  (is-true (ident-code-point-p #\_))
  (is-true (ident-code-point-p #\0))
  (is-true (ident-code-point-p #\-))
  (is-true (ident-code-point-p (code-char #x80)))
  (is-false (ident-code-point-p #\Space))
  (is-false (ident-code-point-p #\()))

;;; consume-ident-sequence tests

(test consume-ident-sequence-simple
  "Test simple identifier"
  (let ((ident (consume-ident-sequence
                (make-string-input-stream "hello"))))
    (is (string= "hello" ident))))

(test consume-ident-sequence-with-digits
  "Test identifier with digits"
  (let ((ident (consume-ident-sequence
                (make-string-input-stream "test123"))))
    (is (string= "test123" ident))))

(test consume-ident-sequence-with-hyphen
  "Test identifier with hyphen"
  (let ((ident (consume-ident-sequence
                (make-string-input-stream "test-name"))))
    (is (string= "test-name" ident))))

(test consume-ident-sequence-with-underscore
  "Test identifier with underscore"
  (let ((ident (consume-ident-sequence
                (make-string-input-stream "_private"))))
    (is (string= "_private" ident))))

(test consume-ident-sequence-stops-at-space
  "Test identifier stops at space"
  (let ((stream (make-string-input-stream "hello world")))
    (let ((ident (consume-ident-sequence stream)))
      (is (string= "hello" ident))
      ;; Space should still be in stream
      (is (char= #\Space (read-char stream))))))

(test consume-ident-sequence-stops-at-paren
  "Test identifier stops at parenthesis"
  (let ((stream (make-string-input-stream "url(")))
    (let ((ident (consume-ident-sequence stream)))
      (is (string= "url" ident))
      ;; Paren should still be in stream
      (is (char= #\( (read-char stream))))))

(test consume-ident-sequence-with-escape
  "Test identifier with escape sequence"
  (let ((ident (consume-ident-sequence
                (make-string-input-stream "hello\\20world"))))
    (is (string= "hello world" ident))))

;;; consume-ident-like-token tests

(test consume-ident-like-token-url-unquoted
  "Test url(...) with unquoted URL"
  (let ((token (consume-ident-like-token
                (make-string-input-stream "url(https://example.com)"))))
    (assert-that token (has-typep 'url-token))
    (is (string= "https://example.com" (token-value token)))))

(test consume-ident-like-token-url-with-whitespace
  "Test url(...) with whitespace before URL"
  (let ((token (consume-ident-like-token
                (make-string-input-stream "url(  https://example.com  )"))))
    (assert-that token (has-typep 'url-token))
    (is (string= "https://example.com" (token-value token)))))

(test consume-ident-like-token-url-case-insensitive
  "Test URL is case-insensitive"
  (let ((token (consume-ident-like-token
                (make-string-input-stream "URL(https://example.com)"))))
    (assert-that token (has-typep 'url-token))
    (is (string= "https://example.com" (token-value token)))))

(test consume-ident-like-token-url-mixed-case
  "Test url with mixed case"
  (let ((token (consume-ident-like-token
                (make-string-input-stream "Url(https://example.com)"))))
    (assert-that token (has-typep 'url-token))
    (is (string= "https://example.com" (token-value token)))))

(test consume-ident-like-token-url-quoted-double
  "Test url(...) with double-quoted string returns function token"
  (let ((token (consume-ident-like-token
                (make-string-input-stream "url(\"https://example.com\")"))))
    (assert-that token (has-typep 'function-token))
    (is (string= "url" (token-value token)))))

(test consume-ident-like-token-url-quoted-single
  "Test url(...) with single-quoted string returns function token"
  (let ((token (consume-ident-like-token
                (make-string-input-stream "url('https://example.com')"))))
    (assert-that token (has-typep 'function-token))
    (is (string= "url" (token-value token)))))

(test consume-ident-like-token-url-quoted-with-whitespace
  "Test url(...) with quoted string and whitespace"
  (let ((token (consume-ident-like-token
                (make-string-input-stream "url(  \"https://example.com\"  )"))))
    (assert-that token (has-typep 'function-token))
    (is (string= "url" (token-value token)))))

(test consume-ident-like-token-url-quoted-stream-position
  "Test that stream is positioned at the quote after consuming url("
  (let ((stream (make-string-input-stream "url(\"test\")")))
    (let ((token (consume-ident-like-token stream)))
      (assert-that token (has-typep 'function-token))
      (is (string= "url" (token-value token)))
      ;; The next character should be the opening quote
      (is (char= #\" (peek-char nil stream))))))

(test consume-ident-like-token-function-errors
  "Test regular function throws error (unimplemented)"
  (signals error
    (consume-ident-like-token
     (make-string-input-stream "rgb(255, 0, 0)"))))

(test consume-ident-like-token-identifier-errors
  "Test identifier throws error (unimplemented)"
  (signals error
    (consume-ident-like-token
     (make-string-input-stream "color"))))
