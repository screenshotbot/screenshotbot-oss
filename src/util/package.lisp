(defpackage #:util
  (:use #:cl
        #:bknr.datastore
        #:hex)
  (:import-from :alexandria
                :assoc-value
                )
  (:import-from :hex
   :acceptor-with-plugins
                :make-url
                :make-prefix-matcher
                :acceptor-plugins
                :make-full-url)
  (:export #:head
           #:google-analytics
           #:safe-redirect
           #:olark
           #:get-request-domain-prefix
           #:access-log
           #:make-url
           #:make-full-url
           #:*disable-sentry*
           #:with-fake-request
           #:load-countries
           #:load-states
           #:dont-send-the-mail
           #:*disable-emails*
           #:prepare-store-for-test
           #:prepare-store
           #:add-get-param-to-url
           #:javascript
           #:base-acceptor
           #:html2text
           #:download-file
           #:bind-form
           #:send-mail
           #:parse-email-list
           #:make-access-log
           #:is-intern?
           #:acceptor-funcall-handler
           #:defview
           #:supports-webp?
           #:acceptor-plugin
           #:acceptor-plugins
           #:acceptor-with-plugins
           #:wrap-template
           #:define-plugin-handler
           #:fix-for-webp
           #:vpush
           #:oid
           #:object-with-oid
           #:object-with-unindexed-oid
           #:file-lock
           #:release-file-lock
           #:find-by-oid
           #:system-source-directory
           #:*acceptor-plugin*
           #:acceptor-plugin-prefix
           #:*object-store*
           #:verify-store
           #:define-css
           #:*delivered-image*
           #:render-view
           #:with-view-builder
           #:tag-manager-head
           #:make-secret-code
           #:tag-manager-body
           #:acceptor-db-config
           #:safe-mp-store
           #:prod-request?
           #:with-html-output
           #:jvm-init
           #:safe-uuid
           #:better-easy-handler
           #:tail))

(defpackage :mquery
  (:use :cl
        :markup
        :alexandria)
  (:export :$
           :attr
           :add-class
   :remove-class
   :has-class-p
           :namequery
           :text
           :parent
           :after
           :val
           :with-document))
