;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :hunchentoot-extensions
  (:nicknames :hex)
  (:use #:cl
        #:alexandria)
  (:import-from #:util/threading
                #:*extras*)
  (:export #:base-acceptor
           #:better-easy-handler
           #:acceptor-funcall-handler
           #:make-full-url
           #:acceptor-plugin-name
           #:acceptor-plugin-prefix
           #:write-postdata-to-file
           #:wrap-template
           #:acceptor-plugin
           #:define-plugin-handler
           #:acceptor-with-plugins
           #:log-crash-extras
           #:*acceptor-plugin*
           #:make-uri-regex
           #:acceptor-db-config
           #:make-uri-regex
           #:add-get-param-to-url
           #:declare-handler
           #:acceptor-plugins
           #:safe-redirect
           #:make-url
           #:supports-webp?
           #:fix-for-webp
           #:prepare-async-response
           #:handle-async-static-file
           #:handle-async-error
           #:register-plugin
           #:process-plugin-request
           #:dispatch-plugin-request
           #:secure-acceptor))
