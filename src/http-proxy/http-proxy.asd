(defsystem :http-proxy
  :serial t
  :depends-on (:hunchentoot
               :util/request
               :util/lru-cache
               :util/digests
               :ironclad
               :quri)
  :components ((:file "server")))

(defsystem :http-proxy/tests
  :serial t
  :depends-on (:http-proxy
               :util/fiveam)
  :components ((:file "test-server")))
