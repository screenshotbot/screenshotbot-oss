(defpackage :screenshotbot/scim/service-provider-config
  (:use #:cl)
  (:import-from #:screenshotbot/scim/users
                #:set-success
                #:defscimhandler)
  (:import-from #:screenshotbot/server
                #:defhandler))
(in-package :screenshotbot/scim/service-provider-config)

(named-readtables:in-readtable :interpol-syntax)


(defhandler (nil :uri "/scim/v2/ServiceProviderConfig" :method :get) ()
  (let ((myurl
          (hex:make-full-url hunchentoot:*request* "/scim/v2/ServiceProviderConfig")))
    (set-success 200)
   #?"{
    \"schemas\":
      [\"urn:ietf:params:scim:schemas:core:2.0:ServiceProviderConfig\"],
    \"documentationUri\": \"http://screenshotbot.io/documentation\",
    \"patch\": {
      \"supported\":false
    },
    \"bulk\": {
      \"supported\":false
    },
    \"filter\": {
      \"supported\":false
    },
    \"changePassword\": {
      \"supported\":false
    },
    \"sort\": {
      \"supported\":false
    },
    \"etag\": {
      \"supported\":false
    },
    \"authenticationSchemes\": [
      {
        \"name\": \"OAuth Bearer Token\",
        \"description\":
          \"Authentication scheme using the OAuth Bearer Token Standard\",
        \"specUri\": \"http://www.rfc-editor.org/info/rfc6750\",
        \"type\": \"oauthbearertoken\",
        \"primary\": true
      }
    ],
    \"meta\": {
      \"location\": \"${myurl}\",
      \"resourceType\": \"ServiceProviderConfig\"
    }
  }
"))
