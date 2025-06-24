;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :remark/test-markdown
  (:use #:cl
        #:fiveam
        #:remark)
  (:import-from #:markup
                #:xml-tag-children)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:local-nicknames (#:a #:alexandria)))
(in-package :remark/test-markdown)

(def-suite* :remark/test-markdown :in :remark)

(named-readtables:in-readtable markup:syntax)

(defun markup= (x y)
  (string=
   (markup:write-html x)
   (markup:write-html y)))

(test markdown
  (is-true <md>foobar</md>))

(test real-content
  (let ((content
          <md>
            Hello world

            This is a test. what do you know. sdfasdf sdf dfsd fsdf sdf dsf
            sdf sdf sdf sd fsd f sdf dsf sdf.
          </md>))
    (is (typep
         content 'markup:xml-merge-tag))
    (let ((children (markup:xml-merge-tag-children content)))
      (is (equal 2 (length children)))
      (is (markup= <p>Hello world</p> (car children))))))


(test headers
  (let ((content
          <md>
            # Hello world

            This is a test. what do you know. sdfasdf sdf dfsd fsdf sdf dsf
            sdf sdf sdf sd fsd f sdf dsf sdf.
          </md>))
    (is (typep
         content 'markup:xml-merge-tag))
    (let ((children (markup:xml-merge-tag-children content)))
      (is (equal 2 (length children)))
      (is (markup= <h1 id= "hello-world" >Hello world</h1> (car children))))))

(test headers-level-3
  (let ((content
          <md>
            ## Hello world

            This is a test. what do you know. sdfasdf sdf dfsd fsdf sdf dsf
            sdf sdf sdf sd fsd f sdf dsf sdf.
          </md>))
    (is (typep
         content 'markup:xml-merge-tag))
    (let ((children (markup:xml-merge-tag-children content)))
      (is (equal 2 (length children)))
      (is (markup= <h2 id= "hello-world" >Hello world</h2> (car children))))))

(markup:deftag code-block (children)
  <div>,@ (progn children)</div>)

(test paragraph-before-code-block
  (let ((content
          <md>
            hello
            
            Once you have this you would still update your <tt>.gitignore</tt>:<code-block>
  **/__Snapshots__/**/*.png
            </code-block>
          </md>))
    (is (typep
         content 'markup:xml-merge-tag))
    (let ((children (markup:xml-merge-tag-children content)))
      (assert-that children
                   ;; There's an additional empty <p> at the bottom for now
                   (has-length 4))
      (is (markup= <p>Once you have this you would still update your <tt>.gitignore</tt>:</p> (second children))))))


