[![Build Status](https://travis-ci.org/joaotavora/hunchensocket.svg?branch=master)](https://travis-ci.org/joaotavora/hunchensocket)
Hunchensocket - WebSockets for Hunchentoot
==========================================

Hunchensocket is a Common Lisp implementation of [WebSocket]s realized
as an extension to [Edi Weitz'] [edi] excellent [Hunchentoot] web
server. Hunchensocket implements a compliant [RFC6455][RFC6455] server. 

Note that Alexander Kahl, the original author, has desactivated his 
[old version][kahl] that only supports the drafts of the protocol.

Installation
------------

Hunchensocket is in [Quicklisp][Quicklisp], so if you have that
setup just do `(ql:quickload :hunchensocket)`.

Quicklisp is also good to use the trunk alongside with other 
dependencies, perhaps to test a new feature or a bugfix:

```
$ cd ~/Source/Lisp/
$ git clone https://github.com/joaotavora/hunchensocket.git
```

```lisp
(push "~/Source/Lisp" ql:*local-project-directories*)
(ql:quickload :hunchensocket) ;; use local hunchensocket and pull
                              ;; dependencies from quicklisp
```

A chat server in 30 lines
-------------------------

First define classes for rooms and users. Make these subclasses of
`websocket-resource` and `websocket-client`.

```lisp
(defpackage :my-chat (:use :cl))
(in-package :my-chat)

(defclass chat-room (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this room!") :reader name))
  (:default-initargs :client-class 'user))

(defclass user (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this user!"))))
```

Define a list of rooms. Notice that
`hunchensocket:*websocket-dispatch-table*` works just like
`hunchentoot:*dispatch-table*`, but for websocket specific resources.

```lisp
(defvar *chat-rooms* (list (make-instance 'chat-room :name "/bongo")
                           (make-instance 'chat-room :name "/fury")))

(defun find-room (request)
  (find (hunchentoot:script-name request) *chat-rooms* :test #'string= :key #'name))

(pushnew 'find-room hunchensocket:*websocket-dispatch-table*)
```

OK, now a helper function and the dynamics of a chat room.

```lisp
(defun broadcast (room message &rest args)
  (loop for peer in (hunchensocket:clients room)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

(defmethod hunchensocket:client-connected ((room chat-room) user)
  (broadcast room "~a has joined ~a" (name user) (name room)))

(defmethod hunchensocket:client-disconnected ((room chat-room) user)
  (broadcast room "~a has left ~a" (name user) (name room)))

(defmethod hunchensocket:text-message-received ((room chat-room) user message)
  (broadcast room "~a says ~a" (name user) message))  
```

Finally, start the server. `hunchensocket:websocket-acceptor` works
just like `hunchentoot:acceptor`, and you can probably also use
`hunchensocket:websocket-ssl-acceptor`.


```lisp
(defvar *server* (make-instance 'hunchensocket:websocket-acceptor :port 12345))
(hunchentoot:start *server*)
```

Now open two browser windows on http://www.websocket.org/echo.html,
enter `ws://localhost:12345/bongo` as the host and play around chatting with
yourself.

License
-------

See [COPYING][copying] for license details.

Design
------

Main sources of inspiration:

* Original implementation by Alexander Kahl, which cleverly hijacks
  the Hunchentoot connection after the HTTP response and keeps the
  connection alive, just like in a Head request.
* [clws][clws]'s API because it explicitly defines websocket "resources"
* [Hunchentoot's][Hunchentoot]'s API because it uses CLOS


[WebSocket]: http://en.wikipedia.org/wiki/WebSocket  
[edi]: http://weitz.de/
[kahl]: https://github.com/e-user/hunchensocket
[RFC6455]: https://tools.ietf.org/html/rfc6455
[clws]: https://github.com/3b/clws
[copying]: https://github.com/joaotavora/hunchensocket/blob/master/COPYING
[Hunchentoot]: http://weitz.de/hunchentoot/
[Quicklisp]: http://www.quicklisp.org/  
