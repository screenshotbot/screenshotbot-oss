;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/css-tokenizer
  (:use #:cl)
  (:export
   #:token
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
   #:ident-code-point-p
   #:consume-escaped-code-point
   #:valid-escape-p))
(in-package :screenshotbot/replay/css-tokenizer)

;;;; As of writing this is an incomplete tokenizer, but based off of
;;;; https://www.w3.org/TR/css-syntax-3/. Good news for us, the CSS
;;;; tokenizer is quite simple and doesn't need a CFG.

(defclass token ()
  ((value :initarg :value
          :reader token-value)))

(defclass url-token (token)
  ())

(defclass bad-url-token (token)
  ())

(defclass string-token (token)
  ())

(defclass bad-string-token (token)
  ())

(defclass function-token (token)
  ())

(defun whitespacep (ch)
  #+lispworks
  (lw:whitespace-char-p ch)
  #-lispworks
  (str:blankp (string ch)))

(defun consume-whitespace (stream)
  (loop while (whitespacep (peek-char nil stream nil))
        do (read-char stream)))

(defun newline-p (ch)
  "Check if character is a newline (U+000A LINE FEED, U+000D CARRIAGE RETURN, or U+000C FORM FEED)"
  (or (char= ch #\Newline)
      (char= ch #\Return)
      (char= ch #\Page)))

(defun non-printable-p (ch)
  "Check if character is non-printable per CSS spec.
Non-printable: U+0000-U+0008, U+000B, U+000E-U+001F, U+007F"
  (let ((code (char-code ch)))
    (or (<= 0 code 8)
        (= code 11)
        (<= 14 code 31)
        (= code 127))))

(defun hex-digit-p (ch)
  "Check if character is a hexadecimal digit"
  (or (char<= #\0 ch #\9)
      (char<= #\A ch #\F)
      (char<= #\a ch #\f)))

(defun letter-p (ch)
  "Check if character is a letter (A-Z or a-z)"
  (or (char<= #\A ch #\Z)
      (char<= #\a ch #\z)))

(defun non-ascii-p (ch)
  "Check if character is non-ASCII (code point >= U+0080)"
  (>= (char-code ch) #x80))

(defun ident-start-p (ch)
  "Check if character is an ident-start code point.
An ident-start code point is: a letter, a non-ASCII code point, or underscore (_)"
  (or (letter-p ch)
      (non-ascii-p ch)
      (char= ch #\_)))

(defun digit-p (ch)
  "Check if character is a digit (0-9)"
  (char<= #\0 ch #\9))

(defun ident-code-point-p (ch)
  "Check if character is an ident code point.
An ident code point is: an ident-start code point, a digit, or hyphen (-)"
  (or (ident-start-p ch)
      (digit-p ch)
      (char= ch #\-)))

(defun valid-escape-p (stream)
  "Check if the next two code points form a valid escape.
A valid escape is backslash followed by anything except newline."
  (let ((ch1 (peek-char nil stream nil))
        (ch2 nil))
    (when (and ch1 (char= ch1 #\\))
      ;; Read the backslash and peek at next char
      (read-char stream)
      (setf ch2 (peek-char nil stream nil))
      ;; Put the backslash back
      (unread-char ch1 stream)
      (and ch2 (not (newline-p ch2))))))

(defun consume-escaped-code-point (stream)
  "Consume an escaped code point. Assumes the backslash has already been consumed."
  (let ((ch (read-char stream nil)))
    (cond
      ((null ch)
       ;; EOF - return replacement character
       (code-char #xFFFD))

      ((hex-digit-p ch)
       ;; Hex escape: consume up to 5 more hex digits
       (let ((hex-string (string ch)))
         (loop repeat 5
               for next-ch = (peek-char nil stream nil)
               while (and next-ch (hex-digit-p next-ch))
               do (progn
                    (setf hex-string (concatenate 'string hex-string (string (read-char stream))))))
         ;; Consume optional whitespace after hex
         (let ((next-ch (peek-char nil stream nil)))
           (when (and next-ch (whitespacep next-ch))
             (read-char stream)))
         ;; Parse hex value
         (let ((code-point (parse-integer hex-string :radix 16)))
           (if (or (= code-point 0)
                   (<= #xD800 code-point #xDFFF)
                   (> code-point #x10FFFF))
               (code-char #xFFFD)
               (code-char code-point)))))

      (t
       ;; Any other character - return as-is
       ch))))

(defun consume-bad-url-remnants (stream)
  "Consume the remnants of a bad URL. Called after detecting a parse error."
  (loop for ch = (read-char stream nil)
        while ch
        do (cond
             ((char= ch #\))
              (return))
             ((char= ch #\\)
              ;; Check for valid escape
              (let ((next-ch (peek-char nil stream nil)))
                (when (and next-ch (not (newline-p next-ch)))
                  ;; Valid escape - consume it
                  (consume-escaped-code-point stream)))))))

(defun consume-ident-sequence (stream)
  "Consume an ident sequence from the stream.
Section 4.3.11 of the standard"
  (let ((result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop
      (let ((ch (peek-char nil stream nil)))
        (cond
          ;; Ident code point - consume and append
          ((and ch (ident-code-point-p ch))
           (read-char stream)
           (vector-push-extend ch result))

          ;; Valid escape sequence - consume escaped code point
          ((and ch (char= ch #\\))
           (read-char stream) ; consume backslash
           (let ((next-ch (peek-char nil stream nil)))
             (cond
               ;; EOF after backslash - put it back and stop
               ((null next-ch)
                (unread-char #\\ stream)
                (return result))
               ;; Newline after backslash - not valid escape, put it back and stop
               ((newline-p next-ch)
                (unread-char #\\ stream)
                (return result))
               ;; Valid escape - consume it
               (t
                (vector-push-extend (consume-escaped-code-point stream) result)))))

          ;; Anything else - stop
          (t
           (return result)))))
    result))

(defun consume-ident-like-token (stream)
  "Consume an ident-like token (identifier, function, or url).
Section 4.3.4 of the standard.
Implemented:
  - url(...) with unquoted URLs -> url-token
  - url(...) with quoted URLs -> function-token
Unimplemented (error):
  - Other function tokens
  - Identifier tokens"
  (let ((string (consume-ident-sequence stream)))
    ;; Check for url function
    (when (and (string-equal string "url")
               (eql (peek-char nil stream nil) #\())
      ;; Consume the left parenthesis
      (read-char stream)
      ;; Consume whitespace
      (consume-whitespace stream)
      ;; Check the next character(s)
      (let ((next-ch (peek-char nil stream nil)))
        (cond
          ;; If it's a quote, it's a function token
          ;; The opening paren has been consumed, but the quoted string remains in the stream
          ((or (eql next-ch #\")
               (eql next-ch #\'))
           (return-from consume-ident-like-token
             (make-instance 'function-token :value string)))

          ;; Otherwise, consume as URL token
          (t
           (return-from consume-ident-like-token (consume-url-token stream))))))

    ;; Check for regular function
    (when (eql (peek-char nil stream nil) #\()
      (error "Unimplemented: function token"))

    ;; Otherwise it's an identifier
    (error "Unimplemented: identifier token")))

(defun consume-url-token (stream)
  "This assumes that the initial `url(` has already been consumed.
Section 4.3.6 of the standard"
  (let ((result (make-instance 'url-token :value ""))
        (url-string (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (consume-whitespace stream)

    (loop
      (let ((ch (read-char stream nil)))
        (cond
          ;; Right parenthesis - done
          ((and ch (char= ch #\)))
           (setf (slot-value result 'value) url-string)
           (return result))

          ;; EOF - parse error but return token
          ((null ch)
           (setf (slot-value result 'value) url-string)
           (return result))

          ;; Whitespace
          ((whitespacep ch)
           (consume-whitespace stream)
           (let ((next-ch (peek-char nil stream nil)))
             (cond
               ;; Followed by ) or EOF - ok
               ((or (null next-ch) (char= next-ch #\)))
                (when next-ch (read-char stream)) ; consume the )
                (setf (slot-value result 'value) url-string)
                (return result))
               ;; Otherwise - bad URL
               (t
                (consume-bad-url-remnants stream)
                (return (make-instance 'bad-url-token :value ""))))))

          ;; Quote or left paren or non-printable - bad URL
          ((or (char= ch #\")
               (char= ch #\')
               (char= ch #\()
               (non-printable-p ch))
           (consume-bad-url-remnants stream)
           (return (make-instance 'bad-url-token :value "")))

          ;; Backslash - check for valid escape
          ((char= ch #\\)
           (let ((next-ch (peek-char nil stream nil)))
             (if (and next-ch (not (newline-p next-ch)))
                 ;; Valid escape - consume it
                 (vector-push-extend (consume-escaped-code-point stream) url-string)
                 ;; Invalid escape - bad URL
                 (progn
                   (consume-bad-url-remnants stream)
                   (return (make-instance 'bad-url-token :value ""))))))

          ;; Any other character - append to URL
          (t
           (vector-push-extend ch url-string)))))))

(defun consume-string-token (stream ending-code-point)
  "Consume a string token. The ending-code-point is the quote character that started the string.
Section 4.3.5 of the standard"
  (let ((result (make-instance 'string-token :value ""))
        (string-value (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop
      (let ((ch (read-char stream nil)))
        (cond
          ;; Ending code point (matching quote) - done
          ((and ch (char= ch ending-code-point))
           (setf (slot-value result 'value) string-value)
           (return result))

          ;; EOF - parse error but return token
          ((null ch)
           (setf (slot-value result 'value) string-value)
           (return result))

          ;; Newline - parse error, reconsume and return bad-string-token
          ((newline-p ch)
           (unread-char ch stream)
           (return (make-instance 'bad-string-token :value "")))

          ;; Backslash - escape sequence
          ((char= ch #\\)
           (let ((next-ch (peek-char nil stream nil)))
             (cond
               ;; EOF after backslash - do nothing
               ((null next-ch)
                nil)

               ;; Newline after backslash - consume it (escaped newline)
               ((newline-p next-ch)
                (read-char stream))

               ;; Valid escape - consume escaped code point
               (t
                (vector-push-extend (consume-escaped-code-point stream) string-value)))))

          ;; Any other character - append to string
          (t
           (vector-push-extend ch string-value)))))))


