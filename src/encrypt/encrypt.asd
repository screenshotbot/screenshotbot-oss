(defsystem encrypt
  :serial t
  :depends-on (:ironclad
               :secure-random
               :util/store
               :cl-base64)
  :components ((:file "encrypt")))

(defsystem encrypt/tests
  :serial t
  :depends-on (:encrypt
               :tmpdir
               :cl-mongo-id)
  :components ((:file "test-encrypt")))
