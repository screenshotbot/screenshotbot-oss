(defsystem :screenshotbot.magick.build
  :serial t
  :depends-on (:str
	       :cl-fad
	       :trivial-features)
  :components ((:file "build")))
