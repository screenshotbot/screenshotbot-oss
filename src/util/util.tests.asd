(defsystem :util.tests
  :depends-on (:util)
  :serial t
  :components ((:module "tests"
                        :components ((:file "test-package")
                                     (:file "test-lists")
                                     (:file "test-models")
                                     (:file "test-cdn")
                                     (:file "test-bind-form")
                                     (:file "test-objectid")
                                     (:file "test-file-lock")
                                     (:file "test-html2text")
                                     (:file "test-mockable")
                                     (:file "test-mquery")))))
