(defpackage :remark/test-markdown
  (:use #:cl
        #:fiveam
        #:remark)
  (:import-from #:markup
                #:xml-tag-children)
  (:local-nicknames (#:a #:alexandria)))
(in-package :remark/test-markdown)

(def-suite :remark/test-markdown :in :remark)

(markup:enable-reader)

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
