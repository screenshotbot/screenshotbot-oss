(in-package :test-util)

(markup:enable-reader)

(test simple-check ()
  (is (equal "hello world
" (html2text <html><body>hello world</body></html>))))
