(defsystem :scale
  :serial t
  :depends-on (:util/request
               :str
               :quri
               :alexandria
               :bordeaux-threads
               :cl-fad)
  :components ((:file "core")
               (:file "linode")
               (:file "vagrant")))
