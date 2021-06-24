(load "scripts/prepare-image")

#+ccl
(ql:quickload "jvm")

#+ccl
(jvm:jvm-init)

(ql:quickload "server")
(ql:quickload "screenshotbot")

(screenshotbot/config:load-config)

(server:main)
