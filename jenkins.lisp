;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;; these quickloads are required because we need to load them before
;; we set dspec:*redefinition-action* to :error

(ql:quickload :test-runner)

(format t "Test runner loaded~%")

(test-runner:init)

#+lispworks
(mp:initialize-multiprocessing :main nil #'test-runner:main)

#-lispworks
(test-runner:main)
