(defpackage :util/statsig
  (:use #:cl)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:alexandria
                #:assoc-value)
  (:local-nicknames (#:json #:yason))
  (:export
   #:statsig-client
   #:make-statsig-client
   #:check-gate
   #:log-event
   #:get-config
   #:get-layer
   #:make-statsig-user))
(in-package :util/statsig)

(defclass statsig-client ()
  ((api-key :initarg :api-key
            :reader api-key
            :documentation "Server-side secret key or client SDK key for Statsig")
   (base-url :initarg :base-url
             :initform "https://api.statsig.com/v1"
             :reader base-url
             :documentation "Base URL for Statsig API")
   (events-url :initarg :events-url
               :initform "https://events.statsigapi.net/v1"
               :reader events-url
               :documentation "Events URL for Statsig API")))

(defun make-statsig-client (api-key &key (base-url "https://api.statsig.com/v1")
                                         (events-url "https://events.statsigapi.net/v1"))
  "Create a new Statsig client with the given API key."
  (make-instance 'statsig-client
                 :api-key api-key
                 :base-url base-url
                 :events-url events-url))

(defun make-statsig-user (&key user-id email ip user-agent country
                               environment custom-ids custom
                               private-attributes)
  "Create a Statsig user object for API requests."
  (let ((user (make-hash-table :test 'equal)))
    (when user-id
      (setf (gethash "userID" user) user-id))
    (when email
      (setf (gethash "email" user) email))
    (when ip
      (setf (gethash "ip" user) ip))
    (when user-agent
      (setf (gethash "userAgent" user) user-agent))
    (when country
      (setf (gethash "country" user) country))
    (when environment
      (setf (gethash "environment" user) environment))
    (when custom-ids
      (setf (gethash "customIDs" user) custom-ids))
    (when custom
      (setf (gethash "custom" user) custom))
    (when private-attributes
      (setf (gethash "privateAttributes" user) private-attributes))
    user))

(defun make-api-headers (client &optional client-time)
  "Create standard headers for Statsig API requests."
  (let ((headers `(("statsig-api-key" . ,(api-key client))
                   ("Content-Type" . "application/json"))))
    (when client-time
      (push `("STATSIG-CLIENT-TIME" . ,(write-to-string client-time)) headers))
    headers))

(defun make-api-request (client endpoint payload &key (use-events-url nil))
  "Make a POST request to the Statsig API."
  (let* ((url (format nil "~a/~a"
                      (if use-events-url
                          (events-url client)
                          (base-url client))
                      endpoint))
         (headers (make-api-headers client))
         (json-payload (json:with-output-to-string* ()
                         (json:encode payload))))
    (log:debug "Payload is: ~a" json-payload)
    (multiple-value-bind (response status response-headers)
        (http-request url
                      :method :post
                      :content json-payload
                      :additional-headers headers
                      :want-string t
                      :ensure-success t)
      (values (json:parse response) status response-headers))))

(defun check-gate (client gate-name user)
  "Check if a feature gate is enabled for the given user.
   Returns a hash table with gate evaluation result."
  (let ((payload (make-hash-table :test 'equal)))
    (setf (gethash "gateName" payload) gate-name)
    (setf (gethash "user" payload) user)
    (make-api-request client "check_gate" payload)))

(defun log-event (client event-name user &key value metadata)
  "Log an event to Statsig analytics.
   EVENT-NAME: Name of the event
   USER: Statsig user object
   VALUE: Optional numeric value 
   METADATA: Optional hash table of additional properties"
  (let* ((event (make-hash-table :test 'equal))
         (events (make-hash-table :test 'equal)))
    (setf (gethash "eventName" event) event-name)
    (setf (gethash "user" event) user)
    (setf (gethash "time" event) (round (/ (- (get-universal-time) 2208988800) 1)))
    (when value
      (setf (gethash "value" event) value))
    (when metadata
      (setf (gethash "metadata" event) metadata))
    (setf (gethash "events" events) (list event))
    (make-api-request client "log_event" events :use-events-url t)))

(defun get-config (client config-name user)
  "Get dynamic config values for the given user.
   Returns a hash table with config values."
  (let ((payload (make-hash-table :test 'equal)))
    (setf (gethash "configName" payload) config-name)
    (setf (gethash "user" payload) user)
    (make-api-request client "get_config" payload)))

(defun get-layer (client layer-name user)
  "Get layer values for the given user.
   Returns a hash table with layer parameter values."
  (let ((payload (make-hash-table :test 'equal)))
    (setf (gethash "layerName" payload) layer-name)
    (setf (gethash "user" payload) user)
    (make-api-request client "get_layer" payload)))


